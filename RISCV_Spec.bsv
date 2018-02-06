package RISCV_Spec;

import ClientServer::*;
import GetPut::*;
import Definitions::*;
import Register_Files::*;
import Memory_BRAM::*;


// ----------------------------------------------------------------------------
// External interface to the mkRISCV_Spec module (not part of the API).
// Allows controlling and inspecting the model state.
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
    Reg#(RISCV_State) state <- mkReg(IDLE);         // Current FSM state
    Reg#(Word) instr_reg    <- mkRegU();            // Instruction register



    // ========================================================================
    // Common idioms for finishing an instruction execution or handling an 
    // exception
    // ========================================================================

    // Standard flow
    function Action finish_normal();
        action
            state <= FETCH;
            pc.write(pc.read() + 4);
        endaction
    endfunction


    // Finish a jump instruction or a branch with condition met
    function Action jump_to(Word target);
        action
            state <= FETCH;
            pc.write(target);
        endaction
    endfunction


    // Halt processor
    function Action halt();
        action
            pc.write(pc.read() + 4);
            state <= IDLE;
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
            trace($format("addi  x%d, x%d, 0x%h", rd, rs1, imm));   
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = signExtend(unpack(imm));
            Word_S result = op_A + op_B;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTI     rd, rs1, imm
    // ------------------------------------------------------------------------
    function Action slti(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("slti  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = signExtend(unpack(imm));
            Word_S result = op_A < op_B ? 1 : 0;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTIU    rd, rs1, imm
    // ------------------------------------------------------------------------
    function Action sltiu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("sltiu  x%d, x%d, 0x%h", rd, rs1, imm));
            Word op_A = gpr.read(rs1);
            Word op_B = zeroExtend(imm);
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // ANDI     rd, rs1, imm   
    // ------------------------------------------------------------------------
    function Action andi(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("andi  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = signExtend(unpack(imm));
            Word_S result = op_A & op_B;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // ORI      rd, rs1, imm   
    // ------------------------------------------------------------------------
    function Action ori(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("ori  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = signExtend(unpack(imm));
            Word_S result = op_A | op_B;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // XORI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action xori(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("xori  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = signExtend(unpack(imm));
            Word_S result = op_A ^ op_B;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLLI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action slli(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("slli  x%d, x%d, 0x%h", rd, rs1, imm));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(imm);
            Word result = op << shamt;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRLI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action srli(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("srli  x%d, x%d, 0x%h", rd, rs1, imm));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(imm);
            Word result = op >> shamt;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRAI     rd, rs1, imm    
    // ------------------------------------------------------------------------
    function Action srai(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("srai  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S op = unpack(gpr.read(rs1));
            Shamt shamt = truncate(imm);
            Word_S result = op >> shamt;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LUI      rd, imm    
    // ------------------------------------------------------------------------
    function Action lui(GPR rd, Imm20 imm);
        action
            trace($format("lui  x%d, 0x%h", rd, imm));
            Word_S result = signExtend(unpack({ imm, 12'h000 }));
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // AUIPC    rd, imm    
    // ------------------------------------------------------------------------
    function Action auipc(GPR rd, Imm20 imm);
        action
            trace($format("auipc  x%d, 0x%h", rd, imm));
            Word_S op = signExtend(unpack({ imm, 12'h000 }));
            Word_S current_pc = unpack(pc.read());
            Word_S result = op + current_pc;
            gpr.write(rd, pack(result));
            finish_normal();
        endaction
    endfunction



    // ========================================================================
    // Integer Register-Register Operations
    // ========================================================================

    // ------------------------------------------------------------------------
    // ADD      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action add_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("add  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A + op_B;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLT      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action slt_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("slt  x%d, x%d, x%d", rd, rs1, rs2));
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = unpack(gpr.read(rs2));
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLTU     rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sltu_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sltu  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A < op_B ? 1 : 0;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // AND      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action and_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("and  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A & op_B;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // OR       rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action or_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("or  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A | op_B;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // XOR      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action xor_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("xor  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A ^ op_B;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SLL      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sll_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sll  x%d, x%d, x%d", rd, rs1, rs2));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(gpr.read(rs2));
            Word result = op << shamt;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRL      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action srl_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("srl  x%d, x%d, x%d", rd, rs1, rs2));
            Word op = gpr.read(rs1);
            Shamt shamt = truncate(gpr.read(rs2));
            Word result = op >> shamt;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SUB      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sub_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sub  x%d, x%d, x%d", rd, rs1, rs2));
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);
            Word result = op_A - op_B;
            gpr.write(rd, result);
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SRA      rd, rs1, rs2    
    // ------------------------------------------------------------------------
    function Action sra_r(GPR rd, GPR rs1, GPR rs2);
        action
            trace($format("sra  x%d, x%d, x%d", rd, rs1, rs2));
            Word_S op = unpack(gpr.read(rs1));
            Shamt shamt = truncate(gpr.read(rs2));
            Word_S result = op >> shamt;
            gpr.write(rd, pack(result));
            finish_normal();
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
            trace($format("jal  x%d, 0x%h", rd, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word saved_pc = pc.read() + 4;
            Word_S next_pc = unpack(pc.read()) + offset;
            gpr.write(rd, saved_pc);
            jump_to(pack(next_pc));
            // Generate a misaligned instruction fetch exception if the target 
            // address is not aligned to a four-byte boundary.
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // JALR      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action jalr(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("jalr  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word saved_pc = pc.read() + 4;
            Word next_pc = pack(base + offset);
            next_pc = { next_pc[x_len-1:1], 1'b0 };
            gpr.write(rd, saved_pc);
            jump_to(next_pc);
            // Generate a misaligned instruction fetch exception if the target 
            // address is not aligned to a four-byte boundary.
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BEQ      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action beq(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("beq  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A == op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BNE      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bne(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bne  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A != op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BLT      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action blt(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("blt  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = unpack(gpr.read(rs2));

            if (op_A < op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BLTU      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bltu(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bltu  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A < op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BGE      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bge(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bge  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word_S op_A = unpack(gpr.read(rs1));
            Word_S op_B = unpack(gpr.read(rs2));

            if (op_A >= op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // BGEU      rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action bgeu(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("bgeu  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S offset = signExtend(unpack({ imm, 1'b0 }));
            Word_S jump_target = unpack(pc.read()) + offset;
            Word op_A = gpr.read(rs1);
            Word op_B = gpr.read(rs2);

            if (op_A >= op_B)
                jump_to(pack(jump_target));
            else
                finish_normal();
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
            trace($format("lb  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LH      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lh(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lh  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LW      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lw(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lw  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LBU      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lbu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lbu  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // LHU      rd, rs, offset   
    // ------------------------------------------------------------------------
    function Action lhu(GPR rd, GPR rs1, Imm12 imm);
        action
            trace($format("lhu  x%d, x%d, 0x%h", rd, rs1, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SB       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sb(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sb  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SH       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sh(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sh  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction

    // ------------------------------------------------------------------------
    // SW       rs1, rs2, offset   
    // ------------------------------------------------------------------------
    function Action sw(GPR rs1, GPR rs2, Imm12 imm);
        action
            trace($format("sw  x%d, x%d, 0x%h", rs1, rs2, imm));
            Word_S base = unpack(gpr.read(rs1));
            Word_S offset = signExtend(unpack(imm));
            Word_S effect_addr = base + offset;
            finish_normal();
        endaction
    endfunction



    // ========================================================================
    // FSM
    // ========================================================================

    // ------------------------------------------------------------------------
    // Instruction fetch
    // ------------------------------------------------------------------------ 
    rule fetch (state == FETCH);
        trace($format("\nFETCH instruction from 0x%h", pc.read()));
        memory.imem.request.put(pc.read());
        state <= EXECUTE;
    endrule


    // ------------------------------------------------------------------------
    // Instruction decode and execution
    // ------------------------------------------------------------------------   
    rule decode_and_execute(state == EXECUTE);
        Word instr <- memory.imem.response.get();
        trace($format("EXECUTE: 0x%h", instr));
        instr_reg <= instr;
        Opcode instr_op = instr[op_len-1:0];

        case (instr_op)
            op_IMM:     
                begin
                    IType i = decode_i_type(instr_reg);
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
                                default: halt();
                            endcase     
                        default: halt();
                    endcase
                end

            op_LUI:     
                begin
                    UType u = decode_u_type(instr_reg);
                    lui(u.rd, u.imm31_12);
                end

            op_AUIPC:   
                begin
                    UType u = decode_u_type(instr_reg);
                    auipc(u.rd, u.imm31_12);
                end

            op_REG:     
                begin
                    RType r = decode_r_type(instr_reg);
                    Func10 func = { r.func7, r.func3 };
                    case (func)
                        f10_ADD:    add_r(r.rd, r.rs1, r.rs2);
                        f10_SLT:    slt_r(r.rd, r.rs1, r.rs2);
                        f10_SLTU:   sltu_r(r.rd, r.rs1, r.rs2);
                        f10_AND:    and_r(r.rd, r.rs1, r.rs2);
                        f10_OR:     or_r(r.rd, r.rs1, r.rs2);
                        f10_XOR:    xor_r(r.rd, r.rs1, r.rs2);
                        f10_SLL:    sll_r(r.rd, r.rs1, r.rs2);
                        f10_SRL:    srl_r(r.rd, r.rs1, r.rs2);
                        f10_SUB:    sub_r(r.rd, r.rs1, r.rs2);
                        f10_SRA:    sra_r(r.rd, r.rs1, r.rs2);
                        default:    halt();
                    endcase
                end

            op_JAL:     
                begin
                    JType j = decode_j_type(instr_reg);
                    jal(j.rd, { j.imm20, j.imm19_12, j.imm11, j.imm10_1 });
                end

            op_JALR:    
                begin
                    IType i = decode_i_type(instr_reg);
                    jalr(i.rd, i.rs1, i.imm11_0);
                end

            op_BRANCH:
                begin
                    BType b = decode_b_type(instr_reg);
                    Imm12 imm = { b.imm12, b.imm11, b.imm10_5, b.imm4_1 };
                    case (b.func3)
                        f3_BEQ:     beq(b.rs1, b.rs2, imm);
                        f3_BNE:     bne(b.rs1, b.rs2, imm);
                        f3_BLT:     blt(b.rs1, b.rs2, imm);
                        f3_BGE:     bge(b.rs1, b.rs2, imm);
                        f3_BLTU:    bltu(b.rs1, b.rs2, imm);
                        f3_BGEU:    bgeu(b.rs1, b.rs2, imm);
                        default:    halt();
                    endcase
                end 

            op_LOAD:
                begin
                    IType i = decode_i_type(instr_reg);
                    case (i.func3)
                        f3_LB:      lb(i.rd, i.rs1, i.imm11_0);
                        f3_LH:      lh(i.rd, i.rs1, i.imm11_0);
                        f3_LW:      lw(i.rd, i.rs1, i.imm11_0);
                        f3_LBU:     lbu(i.rd, i.rs1, i.imm11_0);
                        f3_LHU:     lhu(i.rd, i.rs1, i.imm11_0);
                        default:    halt();
                    endcase
               end

            op_STORE: 
                begin
                    SType s = decode_s_type(instr_reg);
                    Imm12 imm = { s.imm11_5, s.imm4_0 };
                    case (s.func3)
                        f3_SB:      sb(s.rs1, s.rs2, imm);
                        f3_SH:      sh(s.rs1, s.rs2, imm);
                        f3_SW:      sw(s.rs1, s.rs2, imm);
                        default:    halt();
                    endcase
               end

            default:    halt();
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
        state <= FETCH;
    endmethod


endmodule

endpackage