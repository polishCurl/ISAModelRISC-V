package Memory_BRAM;

import ClientServer::*;
import BRAM::*;
import Definitions::*;
import Vector::*;



// ============================================================================
// Memory implementation
// ============================================================================
module mkMemory#(Integer instr_mem_size,        // Instruction memory size
                 Integer data_mem_size,         // Data memory size
                 String prog_src_filename)      // File with Instruction memory contents
                (Memory_Ifc);

    // Configure and instantiate instruction BRAM
    BRAM_Configure ibram_cfg = defaultValue;
    ibram_cfg.loadFormat = tagged Hex prog_src_filename;
    ibram_cfg.memorySize = instr_mem_size;
    BRAM1Port#(Addr, Word) ibram <- mkBRAM1Server(ibram_cfg);

    // Configure and instantiate one BRAM module per byte in a word (for 
    // misaligned data memory acesses)
    BRAM_Configure dbram_cfg = defaultValue;
    dbram_cfg.memorySize = data_mem_size;
    Vector#(BYTEENLEN, BRAM1Port#(Addr, Bit#(8))) dbram <- replicateM(
            mkBRAM1Server(dbram_cfg));

    // Last data memory request made
    Reg#(DMem_Req) last_dmem_req <- mkRegU();

    // Convert the request size (e.g. HALFWORD) into memory byte enable signals
    function Vector#(BYTEENLEN, Bool) size2ByteEnables(DMem_Size size);
        case (size)
            DMEM_BYTE:      return unpack(4'b0001);
            DMEM_HALFWORD:  return unpack(4'b0011);
            DMEM_WORD:      return unpack(4'b1111);
            default:        return unpack(4'b0000);
        endcase
    endfunction



    // ------------------------------------------------------------------------
    // Instruction memory
    // ------------------------------------------------------------------------
    interface Server imem;

        interface Put request;
            method Action put(Addr addr);
                ibram.portA.request.put(BRAMRequest {
                    write :             False,
                    responseOnWrite :   False,
                    address :           addr >> byte_sel_len,
                    datain :            ?
                });
            endmethod
        endinterface


        interface Get response;
            method ActionValue#(Mem_Resp) get();
                Word bram_resp <- ibram.portA.response.get();
                Mem_Resp imem_resp = tagged Mem_Resp_Ok bram_resp;
                return imem_resp;
            endmethod
        endinterface

    endinterface



    // ------------------------------------------------------------------------
    // Data memory
    // ------------------------------------------------------------------------
    interface Server dmem;

        interface Put request;
            method Action put(DMem_Req req);

                // Store the request made so that the right BRAM banks are probed
                // for response
                last_dmem_req <= req;

                // Decompose a data memory request into four byte accesses
                Vector#(BYTEENLEN, Bit#(8)) req_vec = unpack(pack(req.write_data));
                Vector#(BYTEENLEN, Bool) byte_enable = size2ByteEnables(req.size);
                
                for (Integer i = 0; i < byte_en_len; i = i + 1) 
                begin
                    // Pick the appropriate BRAM bank 
                    Addr byte_addr = req.addr + fromInteger(i);
                    ByteSel byte_offset = truncate(byte_addr);
                    dbram[byte_offset].portA.request.put(
                        BRAMRequest {
                            write:              req.mem_op == DMEM_WRITE && byte_enable[i],
                            responseOnWrite:    True,
                            address:            byte_addr >> byte_sel_len,
                            datain:             req_vec[i]
                        });
                end
            endmethod
        endinterface


        interface Get response;
             method ActionValue#(Mem_Resp) get();

                // Combine responses from BRAM banks into a single word
                Vector#(BYTEENLEN, Bit#(8)) resp_vec = ?;
                Vector#(BYTEENLEN, Bool) byte_enable = size2ByteEnables(
                    last_dmem_req.size);
                
                for (Integer i = 0; i < byte_en_len; i = i + 1) 
                begin
                    // Pick the appropriate BRAM bank 
                    Addr byte_addr = last_dmem_req.addr + fromInteger(i);
                    ByteSel byte_offset = truncate(byte_addr);
                    resp_vec[i] <- dbram[byte_offset].portA.response.get();
                end

                Mem_Resp resp = tagged Mem_Resp_Ok unpack(pack(resp_vec));
                return resp;
             endmethod
        endinterface

    endinterface
endmodule


endpackage