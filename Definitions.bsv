package Definitions;

import ClientServer::*;

`include "Config.bsv"


// ----------------------------------------------------------------------------
// Exception numbers
// ----------------------------------------------------------------------------
typedef enum {
    EXC_INSTR_ADDR_MISALIGNED           = 0,
    EXC_INSTR_ACCESS_FAULT              = 1, 
    EXC_ILLEGAL_INSTR                   = 2,
    EXC_BREAKPOINT                      = 3,
    EXC_LOAD_ADDR_MISALIGNED            = 4,
    EXC_LOAD_ACCESS_FAULT               = 5
} Exception deriving (Eq, Bits, FShow);



// ============================================================================
// Instruction decoder definitions
// ============================================================================

// ----------------------------------------------------------------------------
// Data types
// ----------------------------------------------------------------------------
typedef 32  XLEN;                       // GPR register width
typedef 32  XNUM;                       // Number of GPRs
typedef 32  ADDRLEN;                    // Address length
typedef 20  LIMMLEN;                    // Large immediate width    
typedef 12  SIMMLEN;                    // Small immediate width
typedef 7   OPLEN;                      // Opcode width
typedef 7   F7LEN;                      // Func7 instruction field width
typedef 3   F3LEN;                      // Func3 instruction field width

typedef TMul#(XLEN,2)       XLEN2;      // Double GPR register width
typedef TLog#(XNUM)         XADRLEN;    // Number of register address bits
typedef TLog#(XLEN)         SHIFTLEN;   // Number of shift amount bits
typedef TDiv#(ADDRLEN,8)    BYTEENLEN;  // Number of byte-enable bits in a word
typedef TLog#(BYTEENLEN)    BYTESELLEN; // Number of byte-select bits

Integer xlen            = valueOf(XLEN);
Integer xlen_2          = valueOf(XLEN2);
Integer adr_len         = valueOf(ADDRLEN);
Integer x_num           = valueOf(XNUM);
Integer x_adr_len       = valueOf(XADRLEN);
Integer shift_len       = valueOf(SHIFTLEN);
Integer op_len          = valueOf(OPLEN);
Integer byte_en_len     = valueOf(BYTEENLEN);
Integer byte_sel_len    = valueOf(BYTESELLEN);

typedef Bit#(XLEN)      Word;           // Word
typedef Int#(XLEN)      WordS;          // Signed word
typedef Bit#(XLEN2)     DWord;          // DoubleWord
typedef Int#(XLEN2)     DWordS;         // Signed doubleword
typedef Bit#(ADDRLEN)   Addr;           // Word
typedef Bit#(SHIFTLEN)  Shamt;          // Shift amount
typedef Bit#(XADRLEN)   GPR;            // GPR address

typedef Bit#(SIMMLEN)       Imm12;      // 12-bit immediate
typedef Bit#(LIMMLEN)       Imm20;      // 20-bit immediate
typedef Bit#(BYTEENLEN)     ByteEn;     // Byte-enable 
typedef Bit#(BYTESELLEN)    ByteSel;    // Byte-select 



// ----------------------------------------------------------------------------
// Common instruction fields
// ----------------------------------------------------------------------------
typedef Bit#(OPLEN)     Opcode;             // Opcode 
typedef Bit#(F7LEN)     Func7;              
typedef Bit#(F3LEN)     Func3;  


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
    Bit#(12)    imm11_0;
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
    Bit#(20)    imm31_12;
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
// Instruction opcodes
// ----------------------------------------------------------------------------
Opcode op_REG       = 7'b011_0011;
Opcode op_LUI       = 7'b011_0111;
Opcode op_AUIPC     = 7'b001_0111;
Opcode op_IMM       = 7'b001_0011;
Opcode op_JAL       = 7'b110_1111;
Opcode op_JALR      = 7'b110_0111;
Opcode op_BRANCH    = 7'b110_0011;
Opcode op_LOAD      = 7'b000_0011;
Opcode op_STORE     = 7'b010_0011;


// ----------------------------------------------------------------------------
// Instruction function codes
// ----------------------------------------------------------------------------
// Register-Register
Func3 f3_ADD            = 3'b000;
Func3 f3_SUB            = 3'b000;
Func3 f3_SLL            = 3'b001;
Func3 f3_SLT            = 3'b010;
Func3 f3_SLTU           = 3'b011;
Func3 f3_XOR            = 3'b100;
Func3 f3_SRL            = 3'b101;
Func3 f3_SRA            = 3'b101;
Func3 f3_OR             = 3'b110;
Func3 f3_AND            = 3'b111;

Func7 f7_SUB_SRA        = 7'b0100000; 
Func7 f7_REG_OTHER      = 7'b0000000;  


// Register-Immediate
Func3 f3_ADDI       = 3'b000;
Func3 f3_SLLI       = 3'b001;
Func3 f3_SLTI       = 3'b010;
Func3 f3_SLTIU      = 3'b011;
Func3 f3_XORI       = 3'b100;
Func3 f3_SRxI       = 3'b101; 
Func3 f3_ORI        = 3'b110;
Func3 f3_ANDI       = 3'b111;
Func7 f7_SRLI       = 7'b000_0000;
Func7 f7_SRAI       = 7'b010_0000;

// Conditional branches
Func3 f3_BEQ        = 3'b000;
Func3 f3_BNE        = 3'b001;
Func3 f3_BLT        = 3'b100;
Func3 f3_BGE        = 3'b101;
Func3 f3_BLTU       = 3'b110;
Func3 f3_BGEU       = 3'b111;

// Loads
Func3 f3_LB         = 3'b000;
Func3 f3_LH         = 3'b001;
Func3 f3_LW         = 3'b010;
Func3 f3_LBU        = 3'b100;
Func3 f3_LHU        = 3'b101;

// Stores
Func3 f3_SB         = 3'b000;
Func3 f3_SH         = 3'b001;
Func3 f3_SW         = 3'b010;

// Multiplies, divisions and remainders (RV32M)
Func3 f3_MUL        = 3'b000;
Func3 f3_MULH       = 3'b001;
Func3 f3_MULHSU     = 3'b010;
Func3 f3_MULHU      = 3'b011;
Func3 f3_DIV        = 3'b100;
Func3 f3_DIVU       = 3'b101;
Func3 f3_REM        = 3'b110;
Func3 f3_REMU       = 3'b111;

Func7 f7_MULDIV     = 7'b000_0001;

// ----------------------------------------------------------------------------
// Per-type instruction decoders
// ----------------------------------------------------------------------------
// R-Type
function RType decodeRType(Word instr);
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
function IType decodeIType(Word instr);
    return IType {
        imm11_0     : instr[31:20],
        rs1         : instr[19:15],
        func3       : instr[14:12],
        rd          : instr[11:7],
        op          : instr[6:0]
    };
endfunction

// S-Type
function SType decodeSType(Word instr);
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
function BType decodeBType(Word instr);
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
function UType decodeUType(Word instr);
    return UType {
        imm31_12    : instr[31:12],
        rd          : instr[11:7],      
        op          : instr[6:0]
    };
endfunction

// J-Type
function JType decodeJType(Word instr);
    return JType {
        imm20       : instr[31],
        imm10_1     : instr[30:21],
        imm11       : instr[20],
        imm19_12    : instr[19:12],
        rd          : instr[11:7],      
        op          : instr[6:0]
    };
endfunction



// ============================================================================
// MEMORY INTERFACE DEFINITONS
// ============================================================================
// Read/Write access type
typedef enum { 
    DMEM_READ, 
    DMEM_WRITE 
} DMem_Op deriving (Eq, Bits, FShow);

// Data sizes for memory access
typedef enum {
    DMEM_BYTE,
    DMEM_HALFWORD,
    DMEM_WORD
} DMem_Size deriving (Eq, Bits, FShow);

// Data memory request type
typedef struct {
    Addr        addr;
    DMem_Op     mem_op;
    DMem_Size   size;
    Word        write_data;     // Only valid if mem_op == WRITE
} DMem_Req deriving (Eq, Bits);

// Data/Instruction memory response type
typedef union tagged {
    Exception   Mem_Resp_Exception;
    Word        Mem_Resp_Ok;
} Mem_Resp deriving (FShow);



// ----------------------------------------------------------------------------
// Memory interface
// ----------------------------------------------------------------------------
interface Memory_Ifc;
    interface Server#(Addr, Mem_Resp) imem;             // Data memory
    interface Server#(DMem_Req, Mem_Resp) dmem;         // Instruction memory
endinterface

  

// ============================================================================
// NON-ARCHITECTURAL DEFINITONS
// ============================================================================

// RISC-V ISA Model FSM states
typedef enum {
    STATE_FETCH, 
    STATE_EXECUTE,
    STATE_STORE_RESPONSE,
    STATE_LOAD_RESPONSE,
    STATE_IDLE
} RISCV_State deriving (Eq, Bits, FShow);

// Show (or don't) output traces
`ifdef TRACE_EXECUTION
    Bool trace_enabled = True;
`else
    Bool trace_enabled = False;
`endif

// Display debug message
function Action trace(Fmt out) = trace_enabled ? $display(out) : noAction;


endpackage