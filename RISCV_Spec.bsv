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
 * Top-level synthesizable RISC-V architecture specification Bluespec 
 * SystemVerilog module.
 *
 * This model includes the RV32IM and RV64IM (user-level) variants of the 
 * architecture.
 */


package RISCV_Spec;

import ClientServer::*;
import GetPut::*;
import Definitions::*;
import Register_Files::*;
import Memory_BRAM::*;


`include "Config.bsv"       // Include the configuration options


// ----------------------------------------------------------------------------
// External interface to the mkRISCV_Spec module. Allows controlling and 
// inspecting the model state.
// ----------------------------------------------------------------------------
interface RISCV_Ifc;
    method Action   reset();
    //method Action   set_gpr(GPR r, Word data);
    //method Word     read_gpr(GPR r);
    //method Action   set_pc(Word data);
    //method Word     read_pc();
endinterface


// ============================================================================
// RISC-V Spec top-level module
// ============================================================================
module mkRISCV_Spec#(Memory_Ifc memory)     // Memory interface implementation
                    (RISCV_Ifc);            // to the module

    // Architectural state
    GPR_Ifc gpr     <- mkGPR();         // General Purpose Register file 
    PC_Ifc pc       <- mkPC();          // Program Counter              


    // Non-architectural state
    Reg#(RISCV_State) state     <- mkReg(STATE_IDLE);   // FSM state
    Reg#(Instr) current_instr   <- mkRegU();            // Instruction register
    Reg#(Addr) mem_access_addr  <- mkRegU();            // Effective address for LD/ST


    // ========================================================================
    // Common idioms for finishing instruction execution or handling an 
    // exception
    // ========================================================================

    // Increment the Program Counter
    function Action incrementPC();
        action
            state <= STATE_FETCH;
            pc.write(pc.read() + 4);
        endaction
    endfunction


    // Jump to the specified taret address
    function Action jumpTo(Addr target);
        action
            state <= STATE_FETCH;
            pc.write(target);
        endaction
    endfunction


    // Request a data memory access
    function Action memAccess(Addr      addr,
                              DMem_Op   access_type, 
                              DMem_Size access_size, 
                              Word      write_data);
        action
            trace($format(fshow(access_type), ":  addr = 0x%h, data = 0x%h, size = ",
                addr, write_data, fshow(access_size)));

            memory.dmem.request.put(DMem_Req {
                addr:       addr,
                mem_op:     access_type,
                size:       access_size,
                write_data: write_data
            });
            mem_access_addr <= addr;

            if (access_type == DMEM_WRITE)
                state <= STATE_STORE_RESPONSE;
            else
                state <= STATE_LOAD_RESPONSE;
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
    `include "Instr_Decoder.bsv"



    // ========================================================================
    // FSM
    // ========================================================================

    // ------------------------------------------------------------------------
    // Instruction fetch
    // ------------------------------------------------------------------------ 
    rule fetch (state == STATE_FETCH);
        trace($format("\nFETCH: addr = 0x%h", pc.read()));
        memory.imem.request.put(pc.read());
        state <= STATE_EXECUTE;
    endrule


    // ------------------------------------------------------------------------
    // Instruction decode and execute
    // ------------------------------------------------------------------------   
    rule decodeAndExecute(state == STATE_EXECUTE);

        IMem_Resp response <- memory.imem.response.get();
        case (response) matches

            // Invalid instruction memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val pc.read());

            // Instruction successfully fetched
            tagged Mem_Resp_Ok .instr: 
                begin
                    trace($format("EXECUTE: instr = 0x%h", instr));
                    current_instr <= instr;
                    instr_decode(instr);
                end
        endcase
    endrule


    // ------------------------------------------------------------------------
    // Wait for a data memory to return a value
    // ------------------------------------------------------------------------ 
    rule loadWait (state == STATE_LOAD_RESPONSE);

        DMem_Resp response <- memory.dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val mem_access_addr);

            // Save the loaded value to the target register
            tagged Mem_Resp_Ok .data: 
                begin
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
    // Wait for an acknowledgement of data store
    // ------------------------------------------------------------------------ 
    rule storeWait (state == STATE_STORE_RESPONSE);

        DMem_Resp response <- memory.dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), tagged Fault_Val mem_access_addr);

            // Successful store
            tagged Mem_Resp_Ok .data: 
                incrementPC();
        endcase        
    endrule


    // ========================================================================
    // External access and control methods
    // ========================================================================

    /*
    method Action set_gpr(GPR r, Word data);
        gpr.write(r, data);
    endmethod


    method Word read_gpr(GPR r);
        return gpr.read(r);
    endmethod


    method Action set_pc(Word data);
        pc.write(data);
    endmethod


    method Word read_pc();
        return pc.read();
    endmethod


    */

    method Action reset();
        pc.write(0);
        state <= STATE_FETCH;
    endmethod


endmodule

endpackage