package RISCV_Spec;

import ClientServer::*;
import GetPut::*;
import Definitions::*;
import Register_Files::*;
import Memory_BRAM::*;

// TO DO: CHECK BIT EXTENTION FOR LOADS

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
    //method Action   run_instr(Word instr);
endinterface


// ============================================================================
// RISC-V Spec top-level module
// ============================================================================
module mkRISCV_Spec#(Memory_Ifc memory)
                    (RISCV_Ifc);

    // Architectural state
    GPR_Ifc gpr     <- mkGPR();         // General Purpose Register file 
    PC_Ifc pc       <- mkPC();          // Program Counter              


    // Non-architectural state
    Reg#(RISCV_State) state     <- mkReg(STATE_IDLE);   // Current FSM state
    Reg#(Word) current_instr    <- mkRegU();            // Instruction register
    Reg#(Word) mem_access_addr  <- mkRegU();            // Effective address for LD/ST


    // ========================================================================
    // Common idioms for finishing an instruction execution or handling an 
    // exception
    // ========================================================================

    // Normal instruction execution finish sequence
    function Action incrementPC();
        action
            state <= STATE_FETCH;
            pc.write(pc.read() + 4);
        endaction
    endfunction


    // Finish a jump instruction, or a branch with condition met
    function Action jumpTo(Addr target);
        action
            state <= STATE_FETCH;
            pc.write(target);
        endaction
    endfunction


    // Finish LD/ST. Request data memory access.
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
    function Action raiseException(Exception exc, Addr fault_pc, Word fault_val);
        action
            trace($format(fshow(exc), ": fault_pc = 0x%h, fault_val = 0x%h",
                fault_pc, fault_val));
            state <= STATE_IDLE;
        endaction
    endfunction



    // ========================================================================
    // Integer Register-Immediate Operations
    // ========================================================================

    // ------------------------------------------------------------------------
    // ADDI     rd, rs1, imm
    // ------------------------------------------------------------------------
    function Action addi(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("addi  x%1d, x%1d, 0x%h", rd, rs1, imm));   
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = signExtend(unpack(imm));
            Word result = pack(op_A + op_B);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTI     rd, rs1, imm
    // ------------------------------------------------------------------------
    function Action slti(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("slti  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = signExtend(unpack(imm));
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTIU    rd, rs1, imm
    // ------------------------------------------------------------------------
    function Action sltiu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("sltiu  x%1d, x%1d, 0x%h", rd, rs1, imm));
            Word op_A = gpr.read(rs1);
            Word op_B = zeroExtend(imm);
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // ANDI     rd, rs1, imm   
    // ------------------------------------------------------------------------
    function Action andi(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("andi  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = signExtend(unpack(imm));
            Word result = pack(op_A & op_B);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // ORI      rd, rs1, imm   
    // ------------------------------------------------------------------------
    function Action ori(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("ori  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = signExtend(unpack(imm));
            Word result = pack(op_A | op_B);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // XORI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action xori(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("xori  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = signExtend(unpack(imm));
            Word result = pack(op_A ^ op_B);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLLI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action slli(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("slli  x%1d, x%1d, 0x%h", rd, rs1, imm));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(imm);
            Word result = op << shamt;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRLI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action srli(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("srli  x%1d, x%1d, 0x%h", rd, rs1, imm));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(imm);
            Word result = op >> shamt;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRAI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action srai(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("srai  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS op = unpack(gpr.read(rs1));
            Shamt shamt = truncate(imm);
            Word result = pack(op >> shamt);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LUI      rd, imm    
    // ------------------------------------------------------------------------
    function Action lui(GPR rd, Imm20 imm);
        action
            trace($format("lui  x%1d, 0x%h", rd, imm));
            Word result = pack(signExtend({ imm, 12'h000 }));
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // AUIPC    rd, imm    
    // ------------------------------------------------------------------------
    function Action auipc(GPR rd, Imm20 imm);
        action
            trace($format("auipc  x%1d, 0x%h", rd, imm));
            WordS op = signExtend(unpack({ imm, 12'h000 }));
            WordS current_pc = unpack(pc.read());
            Word result = pack(op + current_pc);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction



    // ========================================================================
    // Integer Register-Register Operations
    // ========================================================================

    // ------------------------------------------------------------------------
    // ADD      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action addR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("add  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A + op_B;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLT      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sltR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("slt  x%1d, x%1d, x%1d", rd, rs1, rs2));
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = unpack(gpr.read(rs2));
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTU     rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sltuR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sltu  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // AND      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action andR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("and  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A & op_B;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // OR       rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action orR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("or  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A | op_B;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // XOR      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action xorR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("xor  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A ^ op_B;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLL      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sllR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sll  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(gpr.read(rs2));
            Word result = op << shamt;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRL      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action srlR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("srl  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(gpr.read(rs2));
            Word result = op >> shamt;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SUB      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action subR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sub  x%1d, x%1d, x%1d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A - op_B;
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRA      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sraR(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sra  x%1d, x%1d, x%1d", rd, rs1, rs2));
            WordS op = unpack(gpr.read(rs1));
            Shamt shamt = truncate(gpr.read(rs2));
            Word result = pack(op >> shamt);
            gpr.write(rd, result);
            incrementPC();
        endaction
    endfunction



    // ========================================================================
    // Control Transfer Instructions
    // ========================================================================
    
    // ------------------------------------------------------------------------
    // JAL      rd, offset   
    // ------------------------------------------------------------------------
    function Action jal(GPR rd, Imm20 imm);
        action
            trace($format("jal  x%1d, 0x%h", rd, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Word saved_pc = pc.read() + 4;
            Addr next_pc = pack(unpack(pc.read()) + offset);

            if (next_pc[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), next_pc);
            else
                begin
                    gpr.write(rd, saved_pc);
                    jumpTo(next_pc);
                end
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // JALR      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action jalr(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("jalr  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Word saved_pc = pc.read() + 4;
            Addr next_pc = pack(base + offset);
            next_pc = { next_pc[x_len-1:1], 1'b0 };

            if (next_pc[1:0] != 0)
                raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), next_pc);
            else
                begin
                    gpr.write(rd, saved_pc);
                    jumpTo(next_pc);
                end
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BEQ      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action beq(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("beq  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A == op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
                else
                    jumpTo(jump_target);
            else
                incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BNE      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bne(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bne  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A != op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
                else
                    jumpTo(jump_target);
            else
                incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BLT      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action blt(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("blt  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = unpack(gpr.read(rs2));

            if (op_A < op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
                else
                    jumpTo(jump_target);
            else
                incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BLTU      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bltu(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bltu  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A < op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
                else
                    jumpTo(jump_target);
            else
                incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BGE      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bge(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bge  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            WordS op_A = unpack(gpr.read(rs1));
            WordS op_B = unpack(gpr.read(rs2));

            if (op_A >= op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
                else
                    jumpTo(jump_target);
            else
                incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BGEU      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bgeu(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bgeu  x%1d, x%1d, 0x%h", rs1, rs2, imm));
            WordS offset = signExtend(unpack({ imm, 1'b0 }));
            Addr jump_target = pack(unpack(pc.read()) + offset);
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A >= op_B)
                if (jump_target[1:0] != 0)
                    raiseException(EXC_INSTR_ADDR_MISALIGNED, pc.read(), jump_target);
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
    // LB       rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lb(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lb  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            memAccess(effect_addr, DMEM_READ, DMEM_BYTE, ?);
        endaction
    endfunction

    // LB continued (next clock cycle)
    function Action lbCont(Word rdata, GPR rd);
        action
            Int#(8) relevant_bits = unpack(truncate(rdata));
            Word extended = pack(signExtend(relevant_bits));
            gpr.write(rd, extended);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LH      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lh(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lh  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            memAccess(effect_addr, DMEM_READ, DMEM_HALFWORD, ?);
        endaction
    endfunction

    // LH continued (next clock cycle)
    function Action lhCont(Word rdata, GPR rd);
        action
            Int#(16) relevant_bits = unpack(truncate(rdata));
            Word extended = pack(signExtend(relevant_bits));
            gpr.write(rd, extended);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LW      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lw(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lw  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            memAccess(effect_addr, DMEM_READ, DMEM_WORD, ?);
        endaction
    endfunction

    // LW continued (next clock cycle)
    function Action lwCont(Word rdata, GPR rd);
        action
            Int#(32) relevant_bits = unpack(truncate(rdata));
            Word extended = pack(signExtend(relevant_bits));
            gpr.write(rd, extended);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LBU      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lbu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lbu  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            memAccess(effect_addr, DMEM_READ, DMEM_BYTE, ?);
        endaction
    endfunction

    // LBU continued (next clock cycle)
    function Action lbuCont(Word rdata, GPR rd);
        action
            Bit#(8) relevant_bits = truncate(rdata);
            Word extended = zeroExtend(relevant_bits);
            gpr.write(rd, extended);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LHU      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lhu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lhu  x%1d, x%1d, 0x%h", rd, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            memAccess(effect_addr, DMEM_READ, DMEM_HALFWORD, ?);
        endaction
    endfunction

    // LHU- continued (next cycle)
    function Action lhuCont(Word rdata, GPR rd);
        action
            Bit#(16) relevant_bits = truncate(rdata);
            Word extended = zeroExtend(relevant_bits);
            gpr.write(rd, extended);
            incrementPC();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SB       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sb(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sb  x%1d, (x%1d,0x%h)", rs2, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            Word data_to_write = gpr.read(rs2);
            memAccess(effect_addr, DMEM_WRITE, DMEM_BYTE, data_to_write);
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SH       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sh(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sh  x%1d, (x%1d,0x%h)", rs2, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            Word data_to_write = gpr.read(rs2);
            memAccess(effect_addr, DMEM_WRITE, DMEM_HALFWORD, data_to_write);
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SW       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sw(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sw  x%1d, (x%1d,0x%h)", rs2, rs1, imm));
            WordS base = unpack(gpr.read(rs1));
            WordS offset = signExtend(unpack(imm));
            Addr effect_addr = pack(base + offset);
            Word data_to_write = gpr.read(rs2);
            memAccess(effect_addr, DMEM_WRITE, DMEM_WORD, data_to_write);
        endaction
    endfunction



    // ========================================================================
    // Instruction decoder
    // ========================================================================
    function Action instr_decode(Word instr);
        action
            Opcode instr_op = instr[op_len-1:0];
            case (instr_op)
                op_IMM:     
                    begin
                        IType i = decodeIType(instr);
                        case (i.func3)
                            f3_ADDI:    addi(i.rd, i.rs1, i.imm11_0);
                            f3_SLTI:    slti(i.rd, i.rs1, i.imm11_0);
                            f3_SLTIU:   sltiu(i.rd, i.rs1, i.imm11_0);
                            f3_ANDI:    andi(i.rd, i.rs1, i.imm11_0);
                            f3_ORI:     ori(i.rd, i.rs1, i.imm11_0);
                            f3_XORI:    xori(i.rd, i.rs1, i.imm11_0);
                            f3_SLLI:    slli(i.rd, i.rs1, i.imm11_0);        
                            f3_SRxI:    
                                case(i.imm11_0[11:5])
                                    f7_SRLI: srli(i.rd, i.rs1, i.imm11_0);
                                    f7_SRAI: srai(i.rd, i.rs1, i.imm11_0);
                                    default: raiseException(EXC_ILLEGAL_INSTR, 
                                        pc.read(), instr);
                                endcase     
                            default: raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                            instr);
                        endcase
                    end

                op_LUI:     
                    begin
                        UType u = decodeUType(instr);
                        lui(u.rd, u.imm31_12);
                    end

                op_AUIPC:   
                    begin
                        UType u = decodeUType(instr);
                        auipc(u.rd, u.imm31_12);
                    end

                op_REG:     
                    begin
                        RType r = decodeRType(instr);
                        Func10 func = { r.func7, r.func3 };
                        case (func)
                            f10_ADD:    addR(r.rd, r.rs1, r.rs2);
                            f10_SLT:    sltR(r.rd, r.rs1, r.rs2);
                            f10_SLTU:   sltuR(r.rd, r.rs1, r.rs2);
                            f10_AND:    andR(r.rd, r.rs1, r.rs2);
                            f10_OR:     orR(r.rd, r.rs1, r.rs2);
                            f10_XOR:    xorR(r.rd, r.rs1, r.rs2);
                            f10_SLL:    sllR(r.rd, r.rs1, r.rs2);
                            f10_SRL:    srlR(r.rd, r.rs1, r.rs2);
                            f10_SUB:    subR(r.rd, r.rs1, r.rs2);
                            f10_SRA:    sraR(r.rd, r.rs1, r.rs2);
                            default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                            instr);
                        endcase
                    end

                op_JAL:     
                    begin
                        JType j = decodeJType(instr);
                        jal(j.rd, { j.imm20, j.imm19_12, j.imm11, j.imm10_1 });
                    end

                op_JALR:    
                    begin
                        IType i = decodeIType(instr);
                        jalr(i.rd, i.rs1, i.imm11_0);
                    end

                op_BRANCH:
                    begin
                        BType b = decodeBType(instr);
                        Imm12 imm = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
                        case (b.func3)
                            f3_BEQ:     beq(b.rs1, b.rs2, imm);
                            f3_BNE:     bne(b.rs1, b.rs2, imm);
                            f3_BLT:     blt(b.rs1, b.rs2, imm);
                            f3_BGE:     bge(b.rs1, b.rs2, imm);
                            f3_BLTU:    bltu(b.rs1, b.rs2, imm);
                            f3_BGEU:    bgeu(b.rs1, b.rs2, imm);
                            default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                            instr);
                        endcase
                    end 

                op_LOAD:
                    begin
                        IType i = decodeIType(instr);
                        case (i.func3)
                            f3_LB:      lb(i.rd, i.rs1, i.imm11_0);
                            f3_LH:      lh(i.rd, i.rs1, i.imm11_0);
                            f3_LW:      lw(i.rd, i.rs1, i.imm11_0);
                            f3_LBU:     lbu(i.rd, i.rs1, i.imm11_0);
                            f3_LHU:     lhu(i.rd, i.rs1, i.imm11_0);
                            default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                            instr);
                        endcase
                   end

                op_STORE: 
                    begin
                        SType s = decodeSType(instr);
                        Imm12 imm = { s.imm11_5, s.imm4_0 };
                        case (s.func3)
                            f3_SB:      sb(s.rs1, s.rs2, imm);
                            f3_SH:      sh(s.rs1, s.rs2, imm);
                            f3_SW:      sw(s.rs1, s.rs2, imm);
                            default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                            instr);
                        endcase
                   end

                default: raiseException(EXC_ILLEGAL_INSTR, pc.read(), instr);
            endcase
        endaction
    endfunction



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
    // Instruction decode and execution
    // ------------------------------------------------------------------------   
    rule decodeAndExecute(state == STATE_EXECUTE);

        Mem_Resp response <- memory.imem.response.get();
        case (response) matches

            // Invalid instruction memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), mem_access_addr);

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
    // Wait for a load instruction to return a value
    // ------------------------------------------------------------------------ 
    rule loadWait (state == STATE_LOAD_RESPONSE);

        Mem_Resp response <- memory.dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), mem_access_addr);

            // Save the loaded value to the target register
            tagged Mem_Resp_Ok .data: 
                begin
                    IType i = decodeIType(current_instr);
                    case (i.func3)
                        f3_LB:      lbCont(data, i.rd);
                        f3_LH:      lhCont(data, i.rd);
                        f3_LW:      lwCont(data, i.rd);
                        f3_LBU:     lbuCont(data, i.rd);
                        f3_LHU:     lhuCont(data, i.rd);
                        default:    raiseException(EXC_ILLEGAL_INSTR, pc.read(), 
                                        current_instr);
                    endcase
                end
        endcase        
    endrule


    // ------------------------------------------------------------------------
    // Wait for a store instruction to complete
    // ------------------------------------------------------------------------ 
    rule storeWait (state == STATE_STORE_RESPONSE);

        Mem_Resp response <- memory.dmem.response.get();
        case (response) matches

            // Invalid data memory access
            tagged Mem_Resp_Exception .exc_code: 
                raiseException(exc_code, pc.read(), mem_access_addr);

            // Sucessful store
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


    method Action run_instr(Word instr);
        decode(instr);
    endmethod
    */

    method Action reset();
        pc.write(0);
        state <= STATE_FETCH;
    endmethod


endmodule

endpackage