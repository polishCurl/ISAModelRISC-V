package Definitions;

import ClientServer::*;


// ----------------------------------------------------------------------------
// RISC-V Specification FSM states
// ----------------------------------------------------------------------------
typedef enum {
	FETCH, 
	EXECUTE,
	DMEM_RESPONSE,
	IDLE
} RISCV_State deriving (Eq, Bits, FShow);



// ----------------------------------------------------------------------------
// Data types
// ----------------------------------------------------------------------------
typedef 32	XLEN;           			// GPR register width
typedef 32  XNUM;        				// Number of GPRs
typedef 20 	LIMMLEN;					// Large immediate width	
typedef 12 	SIMMLEN;					// Small immediate width
typedef 7 	OPLEN;						// Opcode width
typedef 7 	F7LEN;						// Func7 instruction field width
typedef 3 	F3LEN;						// Func3 instruction field width

typedef TAdd#(F7LEN,F3LEN) 	F10LEN;		// Total instruction function width
typedef TLog#(XNUM)  		XADRLEN;    // Number of register address bits
typedef TLog#(XLEN)     	SHIFTLEN;   // Number of shift amount bits

Integer x_len 		= valueOf(XLEN);
Integer x_num 		= valueOf(XNUM);
Integer x_adr_len 	= valueOf(XADRLEN);
Integer shift_len 	= valueOf(SHIFTLEN);
Integer op_len 		= valueOf(OPLEN);

typedef Bit#(XLEN)		Word;           // Word
typedef Int#(XLEN)      Word_S;         // Signed word
typedef Bit#(SHIFTLEN)	Shamt;       	// Shift amount
typedef Bit#(XADRLEN)   GPR;			// GPR address

typedef Bit#(SIMMLEN)	Imm12;			// 12-bit immediate
typedef Bit#(LIMMLEN)	Imm20;			// 20-bit immediate



// ============================================================================
// INSTRUCTION DECODER DEFINITONS
// ============================================================================

// ----------------------------------------------------------------------------
// Common instruction fields
// ----------------------------------------------------------------------------
typedef Bit#(OPLEN)		Opcode;				// Opcode 
typedef Bit#(F7LEN)		Func7;				
typedef Bit#(F3LEN)		Func3;	
typedef Bit#(F10LEN)	Func10;				// Func7 and Func3 concatenated


// ----------------------------------------------------------------------------
// Instruction types
// ----------------------------------------------------------------------------
typedef struct {
	Func7 		func7;
	GPR 		rs2;
	GPR 		rs1;
	Func3		func3;
	GPR 		rd;
	Opcode		op;
} RType deriving (Bits);


typedef struct {
	Bit#(12) 	imm11_0;
	GPR 		rs1;
	Func3		func3;
	GPR 		rd;
	Opcode		op;
} IType deriving (Bits);


typedef struct {
	Bit#(7) 	imm11_5;
	GPR 		rs2;
	GPR 		rs1;
	Func3		func3;
	Bit#(5) 	imm4_0;
	Opcode		op;
} SType deriving (Bits);


typedef struct {
	Bit#(1) 	imm12;
	Bit#(6) 	imm10_5;
	GPR 		rs2;
	GPR 		rs1;
	Func3		func3;
	Bit#(4) 	imm4_1;
	Bit#(1) 	imm11;
	Opcode		op;
} BType deriving (Bits);


typedef struct {
	Bit#(20) 	imm31_12;
	GPR 		rd;
	Opcode		op;
} UType deriving (Bits);


typedef struct {
	Bit#(1) 	imm20;
	Bit#(10)	imm10_1;
	Bit#(1)		imm11;
	Bit#(8)		imm19_12;
	GPR 		rd;
	Opcode		op;
} JType deriving (Bits);


// ----------------------------------------------------------------------------
// Instruction opcodes
// ----------------------------------------------------------------------------
Opcode op_REG 		= 7'b011_0011;
Opcode op_LUI   	= 7'b011_0111;
Opcode op_AUIPC 	= 7'b001_0111;
Opcode op_IMM 		= 7'b001_0011;
Opcode op_JAL  		= 7'b110_1111;
Opcode op_JALR 		= 7'b110_0111;
Opcode op_BRANCH 	= 7'b110_0011;
Opcode op_LOAD 		= 7'b000_0011;
Opcode op_STORE 	= 7'b010_0011;


// ----------------------------------------------------------------------------
// Instruction function codes
// ----------------------------------------------------------------------------
// Register-Register
Func10 f10_ADD  	= 10'b00_0000_0000;
Func10 f10_SUB  	= 10'b01_0000_0000;
Func10 f10_SLL  	= 10'b00_0000_0001;
Func10 f10_SLT  	= 10'b00_0000_0010;
Func10 f10_SLTU 	= 10'b00_0000_0011;
Func10 f10_XOR  	= 10'b00_0000_0100;
Func10 f10_SRL  	= 10'b00_0000_0101;
Func10 f10_SRA  	= 10'b01_0000_0101;
Func10 f10_OR  		= 10'b00_0000_0110;
Func10 f10_AND  	= 10'b00_0000_0111;

// Register-Immediate
Func3 f3_ADDI  		= 3'b000;
Func3 f3_SLLI  		= 3'b001;
Func3 f3_SLTI  		= 3'b010;
Func3 f3_SLTIU 		= 3'b011;
Func3 f3_XORI  		= 3'b100;
Func3 f3_SRxI  		= 3'b101; 
Func3 f3_ORI   		= 3'b110;
Func3 f3_ANDI  		= 3'b111;
Func7 f7_SRLI		= 7'b000_0000;
Func7 f7_SRAI		= 7'b010_0000;

// Conditional branches
Func3 f3_BEQ   		= 3'b000;
Func3 f3_BNE   		= 3'b001;
Func3 f3_BLT   		= 3'b100;
Func3 f3_BGE   		= 3'b101;
Func3 f3_BLTU  		= 3'b110;
Func3 f3_BGEU  		= 3'b111;

// Loads
Func3 f3_LB   		= 3'b000;
Func3 f3_LH   		= 3'b001;
Func3 f3_LW   		= 3'b010;
Func3 f3_LBU   		= 3'b100;
Func3 f3_LHU  		= 3'b101;

// Stores
Func3 f3_SB   		= 3'b000;
Func3 f3_SH   		= 3'b001;
Func3 f3_SW   		= 3'b010;




// ----------------------------------------------------------------------------
// Per-type instruction decoders
// ----------------------------------------------------------------------------
function RType decode_r_type(Word instr);
	return RType {
		func7 		: instr[31:25],
		rs2			: instr[24:20],
		rs1			: instr[19:15],
		func3 		: instr[14:12],
		rd 			: instr[11:7],
		op 			: instr[6:0]
	};
endfunction


function IType decode_i_type(Word instr);
	return IType {
		imm11_0 	: instr[31:20],
		rs1			: instr[19:15],
		func3 		: instr[14:12],
		rd 			: instr[11:7],
		op 			: instr[6:0]
	};
endfunction


function SType decode_s_type(Word instr);
	return SType {
		imm11_5 	: instr[31:25],
		rs2			: instr[24:20],
		rs1			: instr[19:15],
		func3 		: instr[14:12],
		imm4_0 		: instr[11:7],
		op 			: instr[6:0]
	};
endfunction


function BType decode_b_type(Word instr);
	return BType {
		imm12 		: instr[31],
		imm10_5		: instr[30:25],
		rs2			: instr[24:20],
		rs1			: instr[19:15],
		func3 		: instr[14:12],
		imm4_1		: instr[11:8],
		imm11 		: instr[7],
		op 			: instr[6:0]
	};
endfunction


function UType decode_u_type(Word instr);
	return UType {
		imm31_12	: instr[31:12],
		rd 			: instr[11:7],		
		op 			: instr[6:0]
	};
endfunction


function JType decode_j_type(Word instr);
	return JType {
		imm20 		: instr[31],
		imm10_1		: instr[30:21],
		imm11 		: instr[20],
		imm19_12	: instr[19:12],
		rd 			: instr[11:7],		
		op 			: instr[6:0]
	};
endfunction



// ============================================================================
// MEMORY INTERFACE DEFINITONS
// ============================================================================

// Types of data memory requests
typedef enum { 
	READ, 
	WRITE 
} DMem_Op deriving (Eq, Bits, FShow);


// Allowed data sizes for memory access
typedef enum {
	BITS_8,
	BITS_16,
	BITS_32,
	BITS_64    // Even in RV32, to allow for Double (floating point)
} DMem_Size deriving (Eq, Bits, FShow);


// Data memory request type
typedef struct {
	DMem_Op 	mem_op;
	DMem_Size 	size;
	Word 		addr;
	Word 		write_data;		// Only valid if mem_op == WRITE
} DMem_Req;


// ----------------------------------------------------------------------------
// Memory interface
// ----------------------------------------------------------------------------
interface Memory_Ifc;
	interface Server#(Word, Word) imem;
	//interface Server#(DMem_Req, Word) dmem;
endinterface

  

// ============================================================================
// FOR DEBUGGING
// ============================================================================

// Show output traces
Bool trace_enabled = True;

// Display debug message
function Action trace(Fmt out) = trace_enabled ? $display(out) : noAction;





endpackage