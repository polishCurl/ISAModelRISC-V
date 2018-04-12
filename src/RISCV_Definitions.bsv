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
 * Architecture-describing definition for RV32IM and RV64IM.
 *
 * This file defines data types, constants and helper functions according to 
 * "The RISC-V Instruction Set Manual Volume I: User-Level ISE Document Version 2.2"
 */


package RISCV_Definitions;


import RegFile::*;              // Standard libraries

`include "Config.bsv"           // Include the configuration options



// ----------------------------------------------------------------------------
// Bit-widths of various architectural parameters
// ----------------------------------------------------------------------------
`ifdef RV32                             // GPR register width and supported 
    typedef 32  XLEN;                   // address space
`elsif RV64
    typedef 64  XLEN;                   
`endif        

typedef 32  XNUM;                       // Number of GPRs
typedef 20  LIMMLEN;                    // Large immediate    
typedef 12  SIMMLEN;                    // Small immediate 
typedef 7   OPLEN;                      // Opcode
typedef 7   F7LEN;                      // Func7 instruction field 
typedef 3   F3LEN;                      // Func3 instruction field 
typedef 32  INSTRLEN;                   // Instruction 
typedef 64  CSRLEN;                     // CSR 
typedef 12  CSRADRLEN;                  // CSR address
typedef 8   BYTELEN;   

typedef TDiv#(XLEN,2)           XLENH;          // Half data bus width
typedef TMul#(XLEN,2)           XLEND;          // Double data bus width
typedef TLog#(XNUM)             XADRLEN;        // Register address
typedef TLog#(XLEN)             SHIFTLEN;       // Shift amount for a data word
typedef TSub#(SHIFTLEN,1)       SHIFTLENH;      // Shift amount for a data halfword
typedef TDiv#(XLEN,8)           BYTESWORD;      // Number of bytes in a data word
typedef TDiv#(INSTRLEN,8)       BYTESINSTR;     // Number of bytes in an instruction
typedef TLog#(BYTESWORD)        WORDBYTESEL;    // Byte-select length for a word
typedef TLog#(BYTESINSTR)       INSTRBYTESEL;   // Byte-select length for an instruction

Integer xlen            = valueOf(XLEN);
Integer xlen_2          = valueOf(XLEND);
Integer xlen_half       = valueOf(XLENH);
Integer adr_len         = valueOf(XLEN);
Integer instr_len       = valueOf(INSTRLEN);
Integer x_num           = valueOf(XNUM);
Integer x_adr_len       = valueOf(XADRLEN);
Integer shift_len       = valueOf(SHIFTLEN);
Integer op_len          = valueOf(OPLEN);
Integer bytes_in_word   = valueOf(BYTESWORD);
Integer bytes_in_instr  = valueOf(BYTESINSTR);
Integer byte_sel_word   = valueOf(WORDBYTESEL);
Integer byte_sel_instr  = valueOf(INSTRBYTESEL);
Integer csr_adr_len     = valueOf(CSRADRLEN);


// ----------------------------------------------------------------------------
// Data types
// ----------------------------------------------------------------------------
// XLEN-dependent data types
typedef Bit#(XLEN)          Word;           // Data word
typedef Int#(XLEN)          WordS;          // Signed data word
typedef Bit#(XLENH)         HWord;          // Data halfword
typedef Int#(XLENH)         HWordS;         // Signed data halfword
typedef Bit#(XLEND)         DWord;          // Data doubleword
typedef Int#(XLEND)         DWordS;         // Signed data doubleword
typedef Bit#(XLEN)          Addr;           // Address

// Fixed-width data types
typedef Bit#(BYTELEN)       Byte;           // Byte
typedef Int#(BYTELEN)       ByteS;          // Signed byte
typedef Bit#(INSTRLEN)      Instr;          // Instruction
typedef Bit#(SHIFTLEN)      Shamt;          // Shift amount for Data
typedef Bit#(SHIFTLENH)     ShamtH;         // Shift amount (halfword)
typedef Bit#(XADRLEN)       GPR;            // GPR address
typedef Bit#(CSRADRLEN)     CSRAddr;        // CSRAddraddress
typedef Bit#(OPLEN)         Opcode;         // Opcode 
typedef Bit#(F7LEN)         Func7;          // 7-bit function code      
typedef Bit#(F3LEN)         Func3;          // 3-bit function code 
typedef Bit#(SIMMLEN)       Imm12;          // 12-bit immediate
typedef Bit#(LIMMLEN)       Imm20;          // 20-bit immediate
typedef Bit#(BYTESWORD)     ByteEn;         // Byte-enable  
typedef Bit#(WORDBYTESEL)   ByteSel;        // Byte-select 

// Register types
typedef RegFile#(GPR, Word) GPRFile;        // General Purpose Register File
typedef Reg#(Addr)          PC;             // Program Counter
typedef Reg#(Bit#(CSRLEN))  CSR;            // Control and Status Register



// ----------------------------------------------------------------------------
// Instruction types
// ----------------------------------------------------------------------------
// R-Type
typedef struct {
    Func7       func7;
    GPR         rs2;
    GPR         rs1;
    Func3       func3;
    GPR         rd;
    Opcode      op;
} RType deriving (Bits);

// I-Type
typedef struct {
    Imm12       imm11_0;
    GPR         rs1;
    Func3       func3;
    GPR         rd;
    Opcode      op;
} IType deriving (Bits);

// S-Type
typedef struct {
    Bit#(7)     imm11_5;
    GPR         rs2;
    GPR         rs1;
    Func3       func3;
    Bit#(5)     imm4_0;
    Opcode      op;
} SType deriving (Bits);

// B-Type
typedef struct {
    Bit#(1)     imm12;
    Bit#(6)     imm10_5;
    GPR         rs2;
    GPR         rs1;
    Func3       func3;
    Bit#(4)     imm4_1;
    Bit#(1)     imm11;
    Opcode      op;
} BType deriving (Bits);

// U-Type
typedef struct {
    Imm20       imm31_12;
    GPR         rd;
    Opcode      op;
} UType deriving (Bits);

// J-Type
typedef struct {
    Bit#(1)     imm20;
    Bit#(10)    imm10_1;
    Bit#(1)     imm11;
    Bit#(8)     imm19_12;
    GPR         rd;
    Opcode      op;
} JType deriving (Bits);



// ----------------------------------------------------------------------------
// Func3 encodings for load instructions (for correct input data processing)
// ----------------------------------------------------------------------------
Func3 f3_LB         = 3'b000;
Func3 f3_LH         = 3'b001;
Func3 f3_LW         = 3'b010;
Func3 f3_LBU        = 3'b100;
Func3 f3_LHU        = 3'b101;


// ----------------------------------------------------------------------------
// Per instruction-type decoders
// ----------------------------------------------------------------------------
// R-Type
function RType decodeRType(Instr instr);
    return RType {
        func7       : instr[31:25],
        rs2         : instr[24:20],
        rs1         : instr[19:15],
        func3       : instr[14:12],
        rd          : instr[11:7],
        op          : instr[6:0]
    };
endfunction

// I-Type
function IType decodeIType(Instr instr);
    return IType {
        imm11_0     : instr[31:20],
        rs1         : instr[19:15],
        func3       : instr[14:12],
        rd          : instr[11:7],
        op          : instr[6:0]
    };
endfunction

// S-Type
function SType decodeSType(Instr instr);
    return SType {
        imm11_5     : instr[31:25],
        rs2         : instr[24:20],
        rs1         : instr[19:15],
        func3       : instr[14:12],
        imm4_0      : instr[11:7],
        op          : instr[6:0]
    };
endfunction

// B-Type
function BType decodeBType(Instr instr);
    return BType {
        imm12       : instr[31],
        imm10_5     : instr[30:25],
        rs2         : instr[24:20],
        rs1         : instr[19:15],
        func3       : instr[14:12],
        imm4_1      : instr[11:8],
        imm11       : instr[7],
        op          : instr[6:0]
    };
endfunction

// U-Type
function UType decodeUType(Instr instr);
    return UType {
        imm31_12    : instr[31:12],
        rd          : instr[11:7],      
        op          : instr[6:0]
    };
endfunction

// J-Type
function JType decodeJType(Instr instr);
    return JType {
        imm20       : instr[31],
        imm10_1     : instr[30:21],
        imm11       : instr[20],
        imm19_12    : instr[19:12],
        rd          : instr[11:7],      
        op          : instr[6:0]
    };
endfunction


// ----------------------------------------------------------------------------
// General Purpose Register (GPR) numbers
// ----------------------------------------------------------------------------
GPR x0  =  0;    GPR x1  =  1;    GPR x2  =  2;    GPR x3  =  3;
GPR x4  =  4;    GPR x5  =  5;    GPR x6  =  6;    GPR x7  =  7;
GPR x8  =  8;    GPR x9  =  9;    GPR x10 = 10;    GPR x11 = 11;
GPR x12 = 12;    GPR x13 = 13;    GPR x14 = 14;    GPR x15 = 15;
GPR x16 = 16;    GPR x17 = 17;    GPR x18 = 18;    GPR x19 = 19;
GPR x20 = 20;    GPR x21 = 21;    GPR x22 = 22;    GPR x23 = 23;
GPR x24 = 24;    GPR x25 = 25;    GPR x26 = 26;    GPR x27 = 27;
GPR x28 = 28;    GPR x29 = 29;    GPR x30 = 30;    GPR x31 = 31;


// ----------------------------------------------------------------------------
// Control and Status register (CSR) addresses
// ----------------------------------------------------------------------------
// RV32 and RV64
CSRAddr csr_addr_cycle      = 12'hc00;
CSRAddr csr_addr_time       = 12'hc01;
CSRAddr csr_addr_instret    = 12'hc02;

// RV32 only
CSRAddr csr_addr_cycleh     = 12'hc80;
CSRAddr csr_addr_timeh      = 12'hc81;
CSRAddr csr_addr_instreth   = 12'hc82;


endpackage