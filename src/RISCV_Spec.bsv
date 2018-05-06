/*-
 * Copyright (c) 2018 Krzysztof Piotr Koch
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


/*-
 * Top-level module for the formal spec of the RISC-V Instruction Set Architecture,
 * written in Bluespec BSV. 
 */


package RISCV_Spec;


import ClientServer::*;         // Standard libraries
import GetPut::*;
import RISCV_Definitions::*;    // Custom libraries
import Definitions::*;
import Registers::*;
import Memory_BRAM::*;


`include "Config.bsv"           // Include the configuration options


// ----------------------------------------------------------------------------
// External interface to the mkRISCV_Spec module. 
// ----------------------------------------------------------------------------
interface RISCV_Ifc;
    method Action       reset();
    method Action       incrTimeCounter();
    method Action       incrCycleCounter();

`ifdef DEBUG_ACCESS
    // Debug Interface
    method Action       stop();
    method Bool         isStopped();
    method Action       setGPR(GPR r, Word data);
    method Addr         readGPR(GPR r);
    method Action       setPC(Addr addr);
    method Word         readPC();
    method Action       setCSR(CSRAddr csr, Word data);
    method Maybe#(Word) readCSR(CSRAddr csr);
`endif

endinterface


// ============================================================================
// RISC-V Spec top-level module
// ============================================================================
module mkRISCV_Spec#(IMem_Ifc imem,
                     DMem_Ifc dmem)     
                    (RISCV_Ifc);   

    // Architectural state
    GPR_Ifc gpr     <- mkGPR();         // General Purpose Register file 
    PC_Ifc pc       <- mkPC();          // Program Counter  
    CSR_Ifc csr     <- mkCSR();         // Control and Status Registers            

    // Non-architectural state
    Reg#(RISCV_State) state     <- mkReg(STATE_IDLE);   // FSM state
    Reg#(Instr) current_instr   <- mkRegU();            // Instruction register
    Reg#(Addr) dmem_access_addr <- mkRegU();            // Effective address for LD/ST
    Reg #(Bool) stop_req        <- mkReg(False);        // Stop-request from debugger


    // ========================================================================
    // Common idioms for finishing instruction execution or handling an 
    // exception
    // ========================================================================
    // Increment the Program Counter 
    function Action incrementPC();
        action
            state <= STATE_FETCH;
            pc.write(pc.read() + 4);
            csr.instretIncr();
        endaction
    endfunction


    // Jump to the specified target address
    function Action jumpTo(Addr target);
        action
            state <= STATE_FETCH;
            pc.write(target);
            csr.instretIncr();
        endaction   
    endfunction


    // Finish a CSRR(X) instruction execution. The incr_instret controls if the 
    // 'retired instructions' counter is updated
    function Action csrrxFinish(Bool incr_instret);
        action
            state <= STATE_FETCH;
            pc.write(pc.read() + 4);
            if (incr_instret)
                csr.instretIncr();
        endaction
    endfunction


    // Request a memory read
    function Action memRead(Addr      addr,
                            DMemSize size);
        action
            trace($format("MEMORY READ:  addr = 0x%h, size = ", addr, fshow(size)));

            dmem.request.put(DMemReq {
                addr:       addr,
                mem_op:     READ,
                size:       size,
                write_data: ?
            });
            
            dmem_access_addr <= addr;
            state <= STATE_LOAD_RESPONSE;
        endaction
    endfunction


    // Request a memory write
    function Action memWrite(Addr      addr, 
                             DMemSize size, 
                             Word      data);
        action
            trace($format("MEMORY WRITE:  addr = 0x%h, data = 0x%h, size = ",
                addr, data, fshow(size)));

            dmem.request.put(DMemReq {
                addr:       addr,
                mem_op:     WRITE,
                size:       size,
                write_data: data
            });

            dmem_access_addr <= addr;
            state <= STATE_STORE_RESPONSE;
        endaction
    endfunction


    // Trap an exception
    function Action raiseException(Exception exc, Addr fault_pc, ExcCause fault_cause);
        action
            case (fault_cause) matches

                tagged Fault_Instr .fault_instr: 
                    trace($format(fshow(exc), ": fault_pc = 0x%h, fault_instr = 0x%h",
                          fault_pc, fault_instr));

                tagged Fault_Val .fault_val: 
                    trace($format(fshow(exc), ": fault_pc = 0x%h, fault_val = 0x%h",
                          fault_pc, fault_val));
            endcase
            state <= STATE_IDLE;
        endaction
    endfunction



    // ========================================================================
    // Instruction semantics
    // ========================================================================
    `include "Instr_Semantics.bsv"



    // ========================================================================
    // Instruction decoder
    // ========================================================================
    `include "Decoder.bsv"



    // ========================================================================
    // A simple FSM necessary to make the spec executable and synthesizable
    // ========================================================================

    // ------------------------------------------------------------------------
    // Instruction fetch
    // ------------------------------------------------------------------------ 
    rule fetch (state == STATE_FETCH);

        // Fetch the next instruction if there is no pending 'stop' request from
        // the debugger
        if (stop_req) begin
            stop_req <= False;
            state <= STATE_IDLE;
        end
        else begin
            trace($format("\nFETCH: addr = 0x%h", pc.read()));
            imem.request.put(pc.read());
            state <= STATE_EXECUTE;
        end
    endrule


    // ------------------------------------------------------------------------
    // Instruction decode and execute
    // ------------------------------------------------------------------------   
    rule decodeAndExecute(state == STATE_EXECUTE);

        IMemResp response <- imem.response.get();
        case (response) matches

            // Invalid instruction memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val pc.read());

            // Instruction successfully fetched
            tagged Mem_Resp_Ok .instr: begin
                trace($format("EXECUTE: instr = 0x%h", instr));
                current_instr <= instr;
                instr_decode(instr);
            end
        endcase
    endrule


    // ------------------------------------------------------------------------
    // Wait for a 'load' response
    // ------------------------------------------------------------------------ 
    rule loadWait (state == STATE_LOAD_RESPONSE);

        DMemResp response <- dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val dmem_access_addr);

            // Save the loaded value to the target register
            tagged Mem_Resp_Ok .data: begin
                IType i = decodeIType(current_instr);
                case (i.func3)
                    f3_LB:      lbCont(i.rd, data);
                    f3_LH:      lhCont(i.rd, data);
                    f3_LW:      lwCont(i.rd, data);
                    f3_LBU:     lbuCont(i.rd, data);
                    f3_LHU:     lhuCont(i.rd, data);
                    default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                               tagged Fault_Instr current_instr);
                endcase
            end
        endcase        
    endrule


    // ------------------------------------------------------------------------
    // Wait for a 'store' response
    // ------------------------------------------------------------------------ 
    rule storeWait (state == STATE_STORE_RESPONSE);

        DMemResp response <- dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val dmem_access_addr);

            // Successful store
            tagged Mem_Resp_Ok .data: 
                incrementPC();
        endcase        
    endrule



    // ========================================================================
    // External access and control methods
    // ========================================================================
    method Action reset();
        pc.write(`PC_RESET_VAL);
        state <= STATE_FETCH;
    endmethod

`ifdef DEBUG_ACCESS

    // Halt the 'processor'
    method Action stop();
        stop_req <= True;
    endmethod

    // Check if the RISC-V spec is idle
    method Bool isStopped();
        return state == STATE_IDLE;
    endmethod


    // Read or modify a GPR
    method Word readGPR(GPR r);
        return gpr.read(r);
    endmethod

    method Action setGPR(GPR r, Word data);
        gpr.write(r, data);
    endmethod
    

    // Read or modify the PC
    method Action setPC(Addr addr);
        pc.write(addr);
    endmethod

    method Addr readPC();
        return pc.read();
    endmethod


    // Read or modify a CSR
    method Action setCSR(CSRAddr addr, Word data);
        if (csr.isAccessOK(addr, WRITE))
            csr.write(addr, data);
    endmethod

    method Maybe#(Word) readCSR(CSRAddr addr);
        if (csr.isAccessOK(addr, READ))
            return tagged Valid csr.read(addr);
        else
            return tagged Invalid;
    endmethod
`endif

    // Allow an external timer to increment the 'cycle' counter
    method Action incrTimeCounter();
        csr.timeIncr();
    endmethod

    // Increment the cycle counter
    method Action incrCycleCounter();
        csr.cycleIncr();
    endmethod


endmodule

endpackage