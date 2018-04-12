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
 * RISC-V Architectural State:
 *  - General Purpose Registers
 *  - Program Counter
 *  - Control and Status Registers
 */

package Registers;

import RegFile::*;                  // Standard libraries  
import RISCV_Definitions::*;        // Custom libraries
import Definitions::*;

`include "Config.bsv"               // Include the configuration options



// ============================================================================
// General Purpose Registers (GPR)
// ============================================================================
// Interface
interface GPR_Ifc;
    method Action   write(GPR r, Word data);
    method Word     read(GPR r);
endinterface


// Implementation
module mkGPR(GPR_Ifc);

    // Instantiate GPR register file
    GPRFile gpr <- mkRegFileFull;

    // Write to GPR     
    method Action write(GPR r, Word data);
        trace($format("x%1d := 0x%h", r, data));
        if (r != x0)
            gpr.upd(r, data);
    endmethod

    // Read GPR     
    method Word read(GPR r);
        if (r == x0)
            return 0;
        else
            return gpr.sub(r);
    endmethod

endmodule



// ============================================================================
// Program Counter (PC) Interface
// ============================================================================
// Interface
interface PC_Ifc;
    method Action   write(Addr addr);
    method Addr     read();
endinterface


// Implementation
module mkPC(PC_Ifc);

    // Instantiate the Program Counter
    PC pc <- mkRegU;            

    // Write to PC
    method Action write(Addr addr);
        trace($format("pc := 0x%h", addr));
        pc <= addr;
    endmethod

    // Read PC
    method Addr read();
        return pc;
    endmethod

endmodule



// ============================================================================
// Control and Status Registers (CSR) Interface
// ============================================================================
// Interface
interface CSR_Ifc;
    method Action   write(CSRAddr addr, Word data);
    method Word     read(CSRAddr addr);
    method Bool     isAccessOK(CSRAddr addr, RW rw);
    method Action   cycleIncr();
    method Action   timeIncr();
    method Action   instretIncr();
endinterface


// Determine whether the 'instret' counter should be incremented given the
// address in the CSRR(X) instruction currently executed and its CSR register 
// access type (read/write).
function Bool incrInstret (CSRAddr addr, RW rw);
    return rw == READ || (addr != csr_addr_instret && addr != csr_addr_instret);
endfunction


// Implementation
module mkCSR(CSR_Ifc);

    // CSR registers defined by the User-Level ISA (all are read only)
    CSR csr_cycle       <- mkReg(0);        // Number of clock cycles executed
    CSR csr_time        <- mkReg(0);        // Wall-clock time 
    CSR csr_instret     <- mkReg(0);        // Number of instructions retired

    CSR csr_scratch     <- mkReg(0);


`ifdef RV32
    // -----------------------------------------------------------------------
    // RV32 CSR interface implementation 
    // -----------------------------------------------------------------------
    // Write to CSR. 
    method Action write(CSRAddr addr, Word data);
        case (addr)         
            csr_addr_cycle:     csr_cycle   <= { csr_cycle[63:32], data };   
            csr_addr_time:      csr_time    <= { csr_time[63:32], data };
            csr_addr_instret:   csr_instret <= { csr_instret[63:32], data };
            csr_addr_cycleh:    csr_cycle   <= { data, csr_cycle[31:0] };
            csr_addr_timeh:     csr_time    <= { data, csr_time[31:0] };
            csr_addr_instreth:  csr_instret <= { data, csr_instret[31:0] };

            12'h000:            csr_scratch <= { csr_scratch[63:32], data };
            12'h001:            csr_scratch <= { data, csr_scratch[31:0] };
        endcase      
        trace($format(showCSR(addr), " := 0x%h", data));   
    endmethod

    // Read CSR
    method Word read(CSRAddr addr);
         case (addr)         
            csr_addr_cycle:     return csr_cycle        [31:0];   
            csr_addr_time:      return csr_time         [31:0]; 
            csr_addr_instret:   return csr_instret      [31:0]; 
            csr_addr_cycleh:    return csr_cycle        [63:32];
            csr_addr_timeh:     return csr_time         [63:32];
            csr_addr_instreth:  return csr_instret      [63:32];

            12'h000:            return csr_scratch[31:0];
            12'h001:            return csr_scratch[63:32];

            default:            return ?;       
        endcase                 
    endmethod


`elsif RV64
    // -----------------------------------------------------------------------
    // RV64 CSR interface implementation 
    // -----------------------------------------------------------------------
    // Write to CSR
    method Action write(CSRAddr addr, Word data);
        case (addr)         
            csr_addr_cycle:     csr_cycle   <= data;   
            csr_addr_time:      csr_time    <= data;
            csr_addr_instret:   csr_instret <= data;
        endcase

        trace($format(showCSR(addr), " := 0x%h", data));   
    endmethod

    // Read CSR
    method Word read(CSRAddr addr);
         case (addr)         
            csr_addr_cycle:     return csr_cycle;   
            csr_addr_time:      return csr_time; 
            csr_addr_instret:   return csr_instret; 
            default:            return ?;       
        endcase                 
    endmethod


`endif
    // -----------------------------------------------------------------------
    // Common CSR interface implementation for both RV32I and RV64I
    // -----------------------------------------------------------------------
    // Check if a CSR exists and can be written to
    method Bool isAccessOK(CSRAddr addr, RW rw);  

        if (addr == 12'h000 || addr == 12'h001)
            return True;

        // All User-level CSRs are read-only
        else if (rw == WRITE)
            return False;
        else
            case (addr)         
                csr_addr_cycle:     return True;   
                csr_addr_time:      return True; 
                csr_addr_instret:   return True; 
`ifdef RV32
                csr_addr_cycleh:    return True;
                csr_addr_timeh:     return True;
                csr_addr_instreth:  return True;
`endif
                default:            return False;       
            endcase      
    endmethod

    // Increment the value of 'cycle' counter
    method Action cycleIncr();
        csr_cycle <= csr_cycle + 1;
    endmethod

    // Increment the value of 'time' counter
    method Action timeIncr();
        csr_time <= csr_time + 1;
    endmethod

    // Increment the value of 'instret' counter
    method Action instretIncr();
        csr_instret <= csr_instret + 1;
    endmethod

endmodule

endpackage