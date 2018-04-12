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
 * Instrution semantics for every instruction in RV32IM and RV64IM 
 * (user-level ISA)
 */



// ========================================================================
// Integer Register-Immediate Operations
// ========================================================================

// ------------------------------------------------------------------------
// ADDI     rd, rs1, imm
// ------------------------------------------------------------------------
function Action addi(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("addi  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));   

        // Execute
        WordS op_A  = unpack(gpr.read(i.rs1));
        WordS op_B  = signExtend(unpack(i.imm11_0));
        Word result = pack(op_A + op_B);
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLTI     rd, rs1, imm
// ------------------------------------------------------------------------
function Action slti(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("slti  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS op_A  = unpack(gpr.read(i.rs1));
        WordS op_B  = signExtend(unpack(i.imm11_0));
        Word result = op_A < op_B ? 1 : 0;
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLTIU    rd, rs1, imm
// ------------------------------------------------------------------------
function Action sltiu(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("sltiu  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        Word op_A   = gpr.read(i.rs1);
        Word op_B   = pack(signExtend(i.imm11_0));
        Word result = op_A < op_B ? 1 : 0;
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// ANDI     rd, rs1, imm   
// ------------------------------------------------------------------------
function Action andi(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("andi  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS op_A  = unpack(gpr.read(i.rs1));
        WordS op_B  = signExtend(unpack(i.imm11_0));
        Word result = pack(op_A & op_B);
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// ORI      rd, rs1, imm   
// ------------------------------------------------------------------------
function Action ori(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("ori  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));
        
        // Execute
        WordS op_A  = unpack(gpr.read(i.rs1));
        WordS op_B  = signExtend(unpack(i.imm11_0));
        Word result = pack(op_A | op_B);
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// XORI     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action xori(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("xori  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS op_A  = unpack(gpr.read(i.rs1));
        WordS op_B  = signExtend(unpack(i.imm11_0));
        Word result = pack(op_A ^ op_B);
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLLI     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action slli(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("slli  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        Word op     = gpr.read(i.rs1);
        Shamt shamt = truncate(i.imm11_0);
        Word result = op << shamt;
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRLI     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action srli(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("srli  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        Word op     = gpr.read(i.rs1);
        Shamt shamt = truncate(i.imm11_0);
        Word result = op >> shamt;
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRAI     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action srai(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("srai  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS op    = unpack(gpr.read(i.rs1));
        Shamt shamt = truncate(i.imm11_0);
        Word result = pack(op >> shamt);
        gpr.write(i.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// LUI      rd, imm    
// ------------------------------------------------------------------------
function Action lui(Instr instr);
    action
        // Decode
        UType u = decodeUType(instr);
        trace($format("lui  x%1d, 0x%h", u.rd, u.imm31_12));

        // Execute
        Word result = pack(signExtend({ u.imm31_12, 12'h000 }));
        gpr.write(u.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// AUIPC    rd, imm    
// ------------------------------------------------------------------------
function Action auipc(Instr instr);
    action
        // Decode
        UType u = decodeUType(instr);
        trace($format("auipc  x%1d, 0x%h", u.rd, u.imm31_12));

        // Execute
        WordS op            = signExtend(unpack({ u.imm31_12, 12'h000 }));
        WordS current_pc    = unpack(pc.read());
        Word result         = pack(op + current_pc);
        gpr.write(u.rd, result);
        incrementPC();
    endaction
endfunction



// ========================================================================
// Integer Register-Register Operations
// ========================================================================

// ------------------------------------------------------------------------
// ADD      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action addR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("add  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A + op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLT      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action sltR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("slt  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        WordS op_A  = unpack(gpr.read(r.rs1));
        WordS op_B  = unpack(gpr.read(r.rs2));
        Word result = op_A < op_B ? 1 : 0;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLTU     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action sltuR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("sltu  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A < op_B ? 1 : 0;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// AND      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action andR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("and  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A & op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// OR       rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action orR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("or  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A | op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// XOR      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action xorR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("xor  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A ^ op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLL      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action sllR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("sll  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op     = gpr.read(r.rs1);
        Shamt shamt = truncate(gpr.read(r.rs2));
        Word result = op << shamt;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRL      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action srlR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("srl  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op     = gpr.read(r.rs1);
        Shamt shamt = truncate(gpr.read(r.rs2));
        Word result = op >> shamt;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SUB      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action subR(Instr instr);
    action
         // Decode
        RType r = decodeRType(instr);
        trace($format("sub  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A   = gpr.read(r.rs1);
        Word op_B   = gpr.read(r.rs2);
        Word result = op_A - op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRA      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action sraR(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("sra  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        WordS op        = unpack(gpr.read(r.rs1));
        Shamt shamt     = truncate(gpr.read(r.rs2));
        Word result     = pack(op >> shamt);
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction



// ========================================================================
// Control Transfer Instructions
// ========================================================================

// ------------------------------------------------------------------------
// JAL      rd, offset   
// ------------------------------------------------------------------------
function Action jal(Instr instr);
    action
        // Decode
        JType j     = decodeJType(instr);
        Imm20 imm   = { j.imm20, j.imm19_12, j.imm11, j.imm10_1 };
        trace($format("jal  x%1d, 0x%h", j.rd, imm));

        // Execute
        Addr current_pc = pc.read();
        WordS offset    = signExtend(unpack({ imm, 1'b0 }));
        Addr saved_pc   = current_pc + 4;
        Addr next_pc    = pack(unpack(current_pc) + offset);

        if (next_pc[1:0] != 0)
            raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                           tagged Fault_Val next_pc);
        else begin
            gpr.write(j.rd, saved_pc);
            jumpTo(next_pc);
        end
    endaction
endfunction

// ------------------------------------------------------------------------
// JALR      rd, rs, offset   
// ------------------------------------------------------------------------
function Action jalr(Instr instr);
    action
        // Decode
        IType i     = decodeIType(instr);
        trace($format("jalr  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        Addr current_pc = pc.read();
        WordS base      = unpack(gpr.read(i.rs1));
        WordS offset    = signExtend(unpack(i.imm11_0));
        Addr saved_pc   = current_pc + 4;
        Addr next_pc    = pack(base + offset);
        next_pc         = { next_pc[xlen-1:1], 1'b0 };

        if (next_pc[1:0] != 0)
            raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                           tagged Fault_Val next_pc);
        else begin
            gpr.write(i.rd, saved_pc);
            jumpTo(next_pc);
        end
    endaction
endfunction

// ------------------------------------------------------------------------
// BEQ      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action beq(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("beq  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        Word op_A           = gpr.read(b.rs1);
        Word op_B           = gpr.read(b.rs2);

        if (op_A == op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// BNE      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action bne(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("bne  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        Word op_A           = gpr.read(b.rs1);
        Word op_B           = gpr.read(b.rs2);

        if (op_A != op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// BLT      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action blt(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("blt  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        WordS op_A          = unpack(gpr.read(b.rs1));
        WordS op_B          = unpack(gpr.read(b.rs2));

        if (op_A < op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// BLTU      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action bltu(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("bltu  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        Word op_A           = gpr.read(b.rs1);
        Word op_B           = gpr.read(b.rs2);

        if (op_A < op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// BGE      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action bge(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("bge  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        WordS op_A          = unpack(gpr.read(b.rs1));
        WordS op_B          = unpack(gpr.read(b.rs2));

        if (op_A >= op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// BGEU      rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action bgeu(Instr instr);
    action
        // Decode
        BType b     = decodeBType(instr);
        Imm12 imm   = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
        trace($format("bgeu  x%1d, x%1d, 0x%h", b.rs1, b.rs2, imm));

        // Execute
        Addr current_pc     = pc.read();
        WordS offset        = signExtend(unpack({ imm, 1'b0 }));
        Addr jump_target    = pack(unpack(current_pc) + offset);
        Word op_A           = gpr.read(b.rs1);
        Word op_B           = gpr.read(b.rs2);

        if (op_A >= op_B)
            if (jump_target[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, current_pc, 
                               tagged Fault_Val jump_target);
            else
                jumpTo(jump_target);
        else
            incrementPC();
    endaction
endfunction



// ========================================================================
// Load and Store Instructions
// ========================================================================

// ------------------------------------------------------------------------
// LB       rd, rs1, offset   
// ------------------------------------------------------------------------
function Action lb(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("lb  x%1d, (x%1d, 0x%h)", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS base          = unpack(gpr.read(i.rs1));
        WordS offset        = signExtend(unpack(i.imm11_0));
        Addr effect_addr    = pack(base + offset);
        memRead(effect_addr, DMEM_BYTE);
    endaction
endfunction

// LB continued (next clock cycle)
function Action lbCont(GPR rd, Word rdata);
    action
        ByteS relevant_bits = unpack(truncate(rdata));
        Word data_to_load   = pack(signExtend(relevant_bits));
        gpr.write(rd, data_to_load);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// LH      rd, rs1, offset   
// ------------------------------------------------------------------------
function Action lh(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("lh  x%1d, (x%1d, 0x%h)", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS base          = unpack(gpr.read(i.rs1));
        WordS offset        = signExtend(unpack(i.imm11_0));
        Addr effect_addr    = pack(base + offset);
        memRead(effect_addr, DMEM_HALFWORD);
    endaction
endfunction

// LH continued (next clock cycle)
function Action lhCont(GPR rd, Word rdata);
    action
        Int#(16) relevant_bits  = unpack(truncate(rdata));
        Word data_to_load       = pack(signExtend(relevant_bits));
        gpr.write(rd, data_to_load);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// LW      rd, rs1, offset   
// ------------------------------------------------------------------------
function Action lw(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("lw  x%1d, (x%1d, 0x%h)", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS base          = unpack(gpr.read(i.rs1));
        WordS offset        = signExtend(unpack(i.imm11_0));
        Addr effect_addr    = pack(base + offset);
        memRead(effect_addr, DMEM_WORD);
    endaction
endfunction

// LW continued (next clock cycle)
function Action lwCont(GPR rd, Word rdata);
    action
        Int#(32) relevant_bits = unpack(truncate(rdata));
        Word data_to_load   = pack(signExtend(relevant_bits));
        gpr.write(rd, data_to_load);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// LBU      rd, rs1, offset   
// ------------------------------------------------------------------------
function Action lbu(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("lbu  x%1d, (x%1d, 0x%h)", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS base          = unpack(gpr.read(i.rs1));
        WordS offset        = signExtend(unpack(i.imm11_0));
        Addr effect_addr    = pack(base + offset);
        memRead(effect_addr, DMEM_BYTE);
    endaction
endfunction

// LBU continued (next clock cycle)
function Action lbuCont(GPR rd, Word rdata);
    action
        Byte relevant_bits  = truncate(rdata);
        Word data_to_load   = zeroExtend(relevant_bits);
        gpr.write(rd, data_to_load);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// LHU      rd, rs, offset   
// ------------------------------------------------------------------------
function Action lhu(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("lhu  x%1d, (x%1d, 0x%h)", i.rd, i.rs1, i.imm11_0));

        // Execute
        WordS base          = unpack(gpr.read(i.rs1));
        WordS offset        = signExtend(unpack(i.imm11_0));
        Addr effect_addr    = pack(base + offset);
        memRead(effect_addr, DMEM_HALFWORD);
    endaction
endfunction

// LHU continued (next cycle)
function Action lhuCont(GPR rd, Word rdata);
    action
        Bit#(16) relevant_bits  = truncate(rdata);
        Word data_to_load       = zeroExtend(relevant_bits);
        gpr.write(rd, data_to_load);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SB       rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action sb(Instr instr);
    action
        // Decode
        SType s = decodeSType(instr);
        Imm12 imm = { s.imm11_5, s.imm4_0 };
        trace($format("sb  x%1d, (x%1d, 0x%h)", s.rs2, s.rs1, imm));

        // Execute
        WordS base          = unpack(gpr.read(s.rs1));
        WordS offset        = signExtend(unpack(imm));
        Addr effect_addr    = pack(base + offset);
        Word data_to_store  = gpr.read(s.rs2);
        memWrite(effect_addr, DMEM_BYTE, data_to_store);
    endaction
endfunction

// ------------------------------------------------------------------------
// SH       rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action sh(Instr instr);
    action
        // Decode
        SType s = decodeSType(instr);
        Imm12 imm = { s.imm11_5, s.imm4_0 };
        trace($format("sh  x%1d, (x%1d, 0x%h)", s.rs2, s.rs1, imm));

        // Execute
        WordS base          = unpack(gpr.read(s.rs1));
        WordS offset        = signExtend(unpack(imm));
        Addr effect_addr    = pack(base + offset);
        Word data_to_store  = gpr.read(s.rs2);
        memWrite(effect_addr, DMEM_HALFWORD, data_to_store);
    endaction
endfunction

// ------------------------------------------------------------------------
// SW       rs1, rs2, offset   
// ------------------------------------------------------------------------
function Action sw(Instr instr);
    action
        // Decode
        SType s = decodeSType(instr);
        Imm12 imm = { s.imm11_5, s.imm4_0 };
        trace($format("sw  x%1d, (x%1d, 0x%h)", s.rs2, s.rs1, imm));

        // Execute
        WordS base          = unpack(gpr.read(s.rs1));
        WordS offset        = signExtend(unpack(imm));
        Addr effect_addr    = pack(base + offset);
        Word data_to_store  = gpr.read(s.rs2);
        memWrite(effect_addr, DMEM_WORD, data_to_store);
    endaction
endfunction



// ========================================================================
// Memory FENCE instructions
// ========================================================================

// ------------------------------------------------------------------------
// FENCE    rd, rs1, pred, succ 
// ------------------------------------------------------------------------
function Action fence(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        Bit#(4) pred        = instr[27:24];
        Bit#(4) succ        = instr[23:20];
        trace($format("fence  x%1d, x%1d, 0x%h, 0x%h", i.rd, i.rs1, pred, succ));

        // Execute
        incrementPC();      // Implemented as NOP
    endaction
endfunction


// ------------------------------------------------------------------------
// FENCE.I  rd, rs1, imm
// ------------------------------------------------------------------------
function Action fencei(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("fence.I  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        incrementPC();      // Implemented as NOP
    endaction
endfunction



// ========================================================================
// Environment Call and Breakpoints
// ========================================================================

// ------------------------------------------------------------------------
// ECALL
// ------------------------------------------------------------------------
function Action ecall(Instr instr);
    action
        // Decode
        trace($format("ecall"));

        // Execute
        raiseException(EXC_ECALL, pc.read(), tagged Fault_Instr instr);
    endaction
endfunction

// ------------------------------------------------------------------------
// EBREAK
// ------------------------------------------------------------------------
function Action ebreak(Instr instr);
    action
        // Decode
        trace($format("ebreak"));

        // Execute
        raiseException(EXC_BREAKPOINT, pc.read(), tagged Fault_Instr instr);
    endaction
endfunction



// ========================================================================
// Control and Status Register Instructions
// ========================================================================

// ------------------------------------------------------------------------
// CSRRW    rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrw(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrw  x%1d, x%1d, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        if (csr.isAccessOK(csr_addr, WRITE)) begin
            if (i.rd != x0) begin
                Word old_csr_val = zeroExtend(csr.read(csr_addr));
                gpr.write(i.rd, old_csr_val);
            end

            Word new_csr_val = gpr.read(i.rs1);
            csr.write(csr_addr, new_csr_val);
            csrrxFinish(incrInstret(csr_addr, WRITE));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);
    endaction
endfunction

// ------------------------------------------------------------------------
// CSRRS    rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrs(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrs  x%1d, x%1d, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        RW access_type = i.rs1 == x0 ? READ : WRITE;

        if (csr.isAccessOK(csr_addr, access_type)) begin
            Word old_csr_val = zeroExtend(csr.read(csr_addr));
            gpr.write(i.rd, old_csr_val);

            if (i.rs1 != x0) begin
                Word bitmask        = gpr.read(i.rs1);
                Word new_csr_val    = old_csr_val | bitmask;
                csr.write(csr_addr, new_csr_val);
            end
            csrrxFinish(incrInstret(csr_addr, access_type));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);       
    endaction
endfunction

// ------------------------------------------------------------------------
// CSRRC    rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrc(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrc  x%1d, x%1d, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        RW access_type = i.rs1 == x0 ? READ : WRITE;

        if (csr.isAccessOK(csr_addr, access_type)) begin
            Word old_csr_val = zeroExtend(csr.read(csr_addr));
            gpr.write(i.rd, old_csr_val);

            if (i.rs1 != x0) begin
                Word bitmask        = gpr.read(i.rs1);
                Word new_csr_val    = old_csr_val & ~bitmask;
                csr.write(csr_addr, new_csr_val);
            end
            csrrxFinish(incrInstret(csr_addr, access_type));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);   
    endaction
endfunction

// ------------------------------------------------------------------------
// CSRRWI    rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrwi(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrwi  x%1d, 0x%h, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        if (csr.isAccessOK(csr_addr, WRITE)) begin

            if (i.rd != x0) begin
                Word old_csr_val = zeroExtend(csr.read(csr_addr));
                gpr.write(i.rd, old_csr_val);
            end

            Word new_csr_val = zeroExtend(i.rs1);
            csr.write(csr_addr, new_csr_val);
            csrrxFinish(incrInstret(csr_addr, WRITE));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);
    endaction
endfunction

// ------------------------------------------------------------------------
// CSRRSI   rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrsi(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrsi  x%1d, 0x%h, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        RW access_type = i.rs1 == x0 ? READ : WRITE;

        if (csr.isAccessOK(csr_addr, access_type)) begin
            Word old_csr_val = zeroExtend(csr.read(csr_addr));
            gpr.write(i.rd, old_csr_val);

            if (i.rs1 != x0) begin
                Word bitmask        = zeroExtend(i.rs1);
                Word new_csr_val    = old_csr_val | bitmask;
                csr.write(csr_addr, new_csr_val);
            end
            csrrxFinish(incrInstret(csr_addr, access_type));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);        
    endaction
endfunction

// ------------------------------------------------------------------------
// CSRRCI   rd, rs1, imm
// ------------------------------------------------------------------------
function Action csrrci(Instr instr);
    action
        // Decode
        IType i             = decodeIType(instr);
        CSRAddr csr_addr    = i.imm11_0;
        trace($format("csrrci  x%1d, 0x%h, 0x%h", i.rd, i.rs1, csr_addr));

        // Execute
        RW access_type = i.rs1 == x0 ? READ : WRITE;

        if (csr.isAccessOK(csr_addr, access_type)) begin
            Word old_csr_val = zeroExtend(csr.read(csr_addr));
            gpr.write(i.rd, old_csr_val);

            if (i.rs1 != x0) begin
                Word bitmask        = zeroExtend(i.rs1);
                Word new_csr_val    = old_csr_val & ~bitmask;
                csr.write(csr_addr, new_csr_val);
            end
            csrrxFinish(incrInstret(csr_addr, access_type));
        end
        else
            raiseException(EXC_ILLEGAL_INSTR, pc.read(), tagged Fault_Instr instr);   
    endaction
endfunction




// ========================================================================
// Multiply and Divide instructions (RV32M)
// ========================================================================
`ifdef RVxxM
// ------------------------------------------------------------------------
// MUL      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action mul(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("mul  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A       = gpr.read(r.rs1);
        Word op_B       = gpr.read(r.rs2);
        Word result     = op_A * op_B;
        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// MULH      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action mulh(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("mulh  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        DWordS op_A     = signExtend(unpack(gpr.read(r.rs1)));
        DWordS op_B     = signExtend(unpack(gpr.read(r.rs2)));
        DWordS result   = op_A * op_B;
        Word highBits   = pack(result)[xlen_2-1:xlen];
        gpr.write(r.rd, highBits);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// MULHSU     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action mulhsu(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("mulhsu  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        DWordS op_A     = signExtend(unpack(gpr.read(r.rs1)));
        DWordS op_B     = unpack(zeroExtend(gpr.read(r.rs2)));
        DWordS result   = op_A * op_B;
        Word highBits   = pack(result)[xlen_2-1:xlen];
        gpr.write(r.rd, highBits);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// MULHU      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action mulhu(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("mulhu  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        DWord op_A      = zeroExtend(gpr.read(r.rs1));
        DWord op_B      = zeroExtend(gpr.read(r.rs2));
        DWord result    = op_A * op_B;
        Word highBits   = result[xlen_2-1:xlen];
        gpr.write(r.rd, highBits);
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// DIV       rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action div(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("div  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        WordS op_A      = unpack(gpr.read(r.rs1));
        WordS op_B      = unpack(gpr.read(r.rs2));
        WordS minus_max = 1 << (xlen - 1);
        WordS result;

        // Divide by 0: rd := -1
        if (op_B == 0)
            result = -1;
        // Overflow: rd := rs1
        else if (op_A == minus_max && op_B == -1)
            result = minus_max;
        else
            result = op_A / op_B;   

        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// DIVU      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action divu(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("divu  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A      = gpr.read(r.rs1);
        Word op_B      = gpr.read(r.rs2);
        Word result;

        // Divide by 0: rd := 2^XLEN - 1
        if (op_B == 0)
            result = signExtend(1'b1);
        else
            result = op_A / op_B;   
                
        gpr.write(r.rd, result);  
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// REM      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action rem(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("rem  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        WordS op_A      = unpack(gpr.read(r.rs1));
        WordS op_B      = unpack(gpr.read(r.rs2));
        WordS minus_max = 1 << (xlen - 1);
        WordS result;

        // Divide by 0: rd := rs1
        if (op_B == 0)
            result = op_A;
        // Overflow: rd := 0
        else if (op_A == minus_max && op_B == -1)
            result = 0;
        else
            result = op_A % op_B;   

        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// REMU     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action remu(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("remu  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        Word op_A      = gpr.read(r.rs1);
        Word op_B      = gpr.read(r.rs2);
        Word result;

        // Divide by 0: rd := rs1
        if (op_B == 0)
            result = op_A;
        else
            result = op_A % op_B;   

        gpr.write(r.rd, result);
        incrementPC();
    endaction
endfunction
`endif



// ========================================================================
// RV64I Base Instruction Set (in addition to RV32I)
// ========================================================================
`ifdef RV64 
// ------------------------------------------------------------------------
// ADDIW     rd, rs1, imm
// ------------------------------------------------------------------------
function Action addiw(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("addiw  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));   

        // Execute
        WordS op_A          = unpack(gpr.read(i.rs1));
        WordS op_B          = signExtend(unpack(i.imm11_0));
        HWordS result_half  = truncate(op_A + op_B);
        WordS result        = signExtend(result_half);
        gpr.write(i.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLLIW     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action slliw(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("slliw  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        HWord op        = truncate(gpr.read(i.rs1));
        ShamtH shamt    = truncate(i.imm11_0);
        WordS result    = signExtend(unpack(op << shamt));
        gpr.write(i.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRLIW     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action srliw(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("srliw  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        HWord op        = truncate(gpr.read(i.rs1));
        ShamtH shamt    = truncate(i.imm11_0);
        WordS result    = signExtend(unpack(op >> shamt));
        gpr.write(i.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRAIW     rd, rs1, imm    
// ------------------------------------------------------------------------
function Action sraiw(Instr instr);
    action
        // Decode
        IType i = decodeIType(instr);
        trace($format("sraiw  x%1d, x%1d, 0x%h", i.rd, i.rs1, i.imm11_0));

        // Execute
        HWordS op       = truncate(unpack(gpr.read(i.rs1)));
        ShamtH shamt    = truncate(i.imm11_0);
        WordS result    = signExtend(op >> shamt);
        gpr.write(i.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// ADDW      rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action addw(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("addw  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        HWord op_A      = truncate(gpr.read(r.rs1));
        HWord op_B      = truncate(gpr.read(r.rs2));
        WordS result    = signExtend(unpack(op_A + op_B));
        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SUBW     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action subw(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("subw  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        HWord op_A      = truncate(gpr.read(r.rs1));
        HWord op_B      = truncate(gpr.read(r.rs2));
        WordS result    = signExtend(unpack(op_A - op_B));
        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SLLW     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action sllw(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("sllw  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        HWord op        = truncate(gpr.read(r.rs1));
        ShamtH shamt    = truncate(gpr.read(r.rs2));
        WordS result    = signExtend(unpack(op << shamt));
        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRLW     rd, rs1, rs2    
// ------------------------------------------------------------------------
function Action srlw(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("srlw  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        HWord op        = truncate(gpr.read(r.rs1));
        ShamtH shamt    = truncate(gpr.read(r.rs2));
        WordS result    = signExtend(unpack(op >> shamt));
        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction

// ------------------------------------------------------------------------
// SRAW      rd, rs1,  rs2    
// ------------------------------------------------------------------------
function Action sraw(Instr instr);
    action
        // Decode
        RType r = decodeRType(instr);
        trace($format("sraw  x%1d, x%1d, x%1d", r.rd, r.rs1, r.rs2));

        // Execute
        HWordS op       = truncate(unpack(gpr.read(r.rs1)));
        ShamtH shamt    = truncate(gpr.read(r.rs2));
        WordS result    = signExtend(op >> shamt);
        gpr.write(r.rd, pack(result));
        incrementPC();
    endaction
endfunction


`endif
