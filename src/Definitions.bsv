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
 * Helper definitions _not_ specified in "The RISC-V Instruction Set Manual
 *  Volume I: User-Level ISE Document Version 2.2",
 *
 * Definition below are used for debugging purposes or to make the 
 * specification synthesizable.
 */

package Definitions;


import ClientServer::*;             // Standard libraries
import RISCV_Definitions::*;        // Custom libraries

`include "Config.bsv"               // Include the configuration options



// ----------------------------------------------------------------------------
// General definitions
// ----------------------------------------------------------------------------
// Access type (read or write)
typedef enum { 
    READ, 
    WRITE 
} RW deriving (Eq, Bits, FShow);


// ----------------------------------------------------------------------------
// Memory interface definitions
// ----------------------------------------------------------------------------
// Instruction and Data BRAM sizes (both in bytes and memory words)
Integer ibram_size_byte = valueOf(`IMEM_SIZE);
Integer ibram_size      = ibram_size_byte / bytes_in_instr;
Integer dbram_size_byte = valueOf(`DMEM_SIZE);
Integer dbram_size      = dbram_size_byte / bytes_in_word;

// Instruction and Data BRAM input address types 
typedef Bit#(TLog#(`IMEM_SIZE)) IBRAMAddr;
typedef Bit#(TLog#(`DMEM_SIZE)) DBRAMAddr;

// Instruction memory response type
typedef union tagged {
    Exception   Mem_Resp_Exception;
    Instr       Mem_Resp_Ok;
} IMemResp deriving (Eq, Bits);

// Allowed sizes for data memory access
typedef enum {
    DMEM_BYTE       = 0,
    DMEM_HALFWORD   = 1,
    DMEM_WORD       = 2,
    DMEM_DOUBLEWORD = 3
} DMemSize deriving (Eq, Bits, FShow);

// Data memory request type
typedef struct {
    Addr        addr;
    RW          mem_op;
    DMemSize    size;
    Word        write_data;     // Only valid if mem_op == WRITE
} DMemReq deriving (Eq, Bits);

// Data memory response type
typedef union tagged {
    Exception   Mem_Resp_Exception;
    Word        Mem_Resp_Ok;
} DMemResp deriving (Eq, Bits);

// Memory interface 
typedef Server#(Addr, IMemResp) IMem_Ifc;       // Instruction memory
typedef Server#(DMemReq, DMemResp) DMem_Ifc;    // Data memory


// ----------------------------------------------------------------------------
// Debug definitions
// ----------------------------------------------------------------------------
// Exception numbers
typedef enum {
    EXC_INSTR_ADDR_MISALIGNED           = 0,
    EXC_INSTR_ACCESS_FAULT              = 1, 
    EXC_ILLEGAL_INSTR                   = 2,
    EXC_BREAKPOINT                      = 3,
    EXC_DATA_ADDR_MISALIGNED            = 4,
    EXC_DATA_ACCESS_FAULT               = 5,
    EXC_ECALL                           = 8
} Exception deriving (Eq, Bits, FShow);

// Exception cause, either an instruction (32-bit) or a data word (32-bit or 64-bit)
typedef union tagged {
    Instr   Fault_Instr;
    Word    Fault_Val;
} ExcCause deriving (FShow);

// Show (or don't) output traces
`ifdef TRACE_EXECUTION
    Bool trace_enabled = True;
`else
    Bool trace_enabled = False;
`endif

// Display debug message
function Action trace(Fmt out) = trace_enabled ? $display(out) : noAction;

// Display a CSR name given its address
function Fmt showCSR (CSRAddr addr);
    case (addr)
        csr_addr_cycle:     return fshow("cycle");   
        csr_addr_time:      return fshow("time"); 
        csr_addr_instret:   return fshow("instret"); 
        csr_addr_cycleh:    return fshow("cycleh");
        csr_addr_timeh:     return fshow("timeh");
        csr_addr_instreth:  return fshow("instreth");
        default:            return fshow("scratch");
    endcase
endfunction


// ----------------------------------------------------------------------------
// RISC-V ISA Model FSM definitions
// ----------------------------------------------------------------------------
// FSM states
typedef enum {
    STATE_FETCH, 
    STATE_EXECUTE,
    STATE_STORE_RESPONSE,
    STATE_LOAD_RESPONSE,
    STATE_IDLE
} RISCV_State deriving (Eq, Bits, FShow);


endpackage