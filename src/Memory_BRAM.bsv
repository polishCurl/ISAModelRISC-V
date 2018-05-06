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
 * Two different memory interface implementations. Both have separate 
 * instruction and data memories. 
 *
 * The two data memory variants both support misaligned accesses. However, the
 * first version has a single BRAM bank and a variable data memory latency
 * which depends on the memory access size.
 * 
 * The alternative data memory implementation consists of a number of byte-wide
 * BRAM banks that are accesses simultaneously. Therefore, the memory latency
 * is constant.
 */


package Memory_BRAM;

import ClientServer::*;         // Standard libraries
import FIFO::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import RISCV_Definitions::*;    // Custom libraries
import Definitions::*;


`include "Config.bsv"           // Include the configuration options



// ============================================================================
// Instruction memory
// ============================================================================
module mkIMem(IMem_Ifc);

    // Configure and instantiate instruction BRAM
    BRAM_Configure ibram_cfg            = defaultValue;
    String imem_file                    = `IMEM_PATH;
    ibram_cfg.loadFormat                = tagged Hex imem_file;
    ibram_cfg.memorySize                = ibram_size;
    BRAM1Port#(IBRAMAddr, Instr) ibram  <- mkBRAM1Server(ibram_cfg);

    // Flag indicating if the request being served is valid
    Reg#(Bool) is_imem_req_valid <- mkRegU();
    
    interface Put request;
        method Action put(Addr addr);

            // Check if the instruction address is aligned and within memory
            if (addr[1:0] != 0 || addr >= `IMEM_SIZE)
                is_imem_req_valid <= False;
            else begin
                ibram.portA.request.put(BRAMRequest {
                    write :             False,
                    responseOnWrite :   False,
                    address :           truncate(addr >> byte_sel_instr),
                    datain :            ?
                });
                is_imem_req_valid <= True;
            end
        endmethod
    endinterface


    interface Get response;
        method ActionValue#(IMemResp) get();
            if (is_imem_req_valid) begin
                Instr resp <- ibram.portA.response.get();
                return tagged Mem_Resp_Ok resp;
            end
            else
                return tagged Mem_Resp_Exception EXC_INSTR_ACCESS_FAULT;

        endmethod
    endinterface
endmodule





// ============================================================================
// Data memory (variable latency)
// ============================================================================
module mkDMemSimple(DMem_Ifc);

    // Configure and instantiate data BRAM. 
    BRAM_Configure dbram_cfg            = defaultValue;
    String dmem_file                    = `DMEM_PATH;
    dbram_cfg.loadFormat                = tagged Hex dmem_file;
    dbram_cfg.memorySize                = dbram_size_byte;
    BRAM1Port#(DBRAMAddr, Byte) dbram   <- mkBRAM1Server(dbram_cfg);

    // Data memory response buffer
    FIFO#(DMemResp) dmem_resp_fifo <- mkFIFO;

    // Byte-vector with memory response
    Reg#(Vector#(BYTESWORD, Byte)) resp_vec <- mkRegU;

    // Currently processed request
    Reg#(DMemReq) current_req <- mkRegU;

    // Flag indicating if the controller is ready to accept a new request       
    Reg#(Bool) busy <- mkReg(False);

    // Index of the data byte being loaded/stored
    Reg#(Bit#(TAdd#(BYTESWORD,1))) byte_index <- mkRegU;


    // ------------------------------------------------------------------------
    // FSM
    // ------------------------------------------------------------------------
    // A data memory request is being served
    rule dmemRequestProcess (busy && byte_index < (1 << pack(current_req.size)));

        // Read the response for the last BRAM request sent
        Byte dbram_resp  <- dbram.portA.response.get();
        resp_vec[byte_index - 1] <= dbram_resp;

        // Issue the next load/store byte request to BRAM
        Addr byte_addr = current_req.addr + zeroExtend(byte_index);
        Vector#(BYTESWORD, Byte) data_vec = unpack(pack(current_req.write_data));
        
        if (byte_addr >= `DMEM_SIZE)
            dmem_resp_fifo.enq(tagged Mem_Resp_Exception EXC_DATA_ACCESS_FAULT);
        else
            dbram.portA.request.put(
                    BRAMRequest {
                        write:              current_req.mem_op == WRITE,
                        responseOnWrite:    True,
                        address:            truncate(byte_addr),
                        datain:             data_vec[byte_index]
                    });

        byte_index <= byte_index + 1;
    endrule

    // Finish serving a data memory request
    rule dmemRequestFinish (busy && byte_index >= (1 << pack(current_req.size)));

        // Read the final byte from data BRAM and form the response
        Vector#(BYTESWORD, Byte) final_resp_vec = resp_vec;
        final_resp_vec[byte_index - 1] <- dbram.portA.response.get();
        busy <= False;
        dmem_resp_fifo.enq(tagged Mem_Resp_Ok unpack(pack(final_resp_vec)));
    endrule



    // ------------------------------------------------------------------------
    // Interface implementation
    // ------------------------------------------------------------------------
    // Split the incoming data memory request into one or more BRAM requests
    // processed sequentially
    interface Put request;
        method Action put(DMemReq req) if (!busy);

            // Convert input data into a vector of bytes
            Vector#(BYTESWORD, Byte) data_vec = unpack(pack(req.write_data));

            // Check if the byte address is valid and send the first data 
            // BRAM request
            if (req.addr >= `DMEM_SIZE)
                dmem_resp_fifo.enq(tagged Mem_Resp_Exception EXC_DATA_ACCESS_FAULT);
            else
                dbram.portA.request.put(
                        BRAMRequest {
                            write:              req.mem_op == WRITE,
                            responseOnWrite:    True,
                            address:            truncate(req.addr),
                            datain:             data_vec[0]
                        });

            // Initialise the memory controller state
            current_req     <= req;
            busy            <= True;
            byte_index      <= 1;
        endmethod
    endinterface

    interface response = toGet(dmem_resp_fifo);
endmodule




// ============================================================================
// Multi-banked data memory
// ============================================================================
module mkDMemMultibanked(DMem_Ifc);

    // Configure and instantiate one BRAM module per byte in a word 
    BRAM_Configure dbram_cfg        = defaultValue;
    dbram_cfg.memorySize            = dbram_size;
    Vector#(BYTESWORD, BRAM1Port#(DBRAMAddr, Byte)) dbram;

    for (Integer i = 0; i < bytes_in_word; i = i + 1) begin
        String dmem_file        = `DMEM_MULTI_PATH + integerToString(i);
        dbram_cfg.loadFormat    = tagged Hex dmem_file;
        dbram[i]                <- mkBRAM1Server(dbram_cfg);
    end

    // Last data memory request made
    Reg#(DMemReq) last_dmem_req <- mkRegU();

    // Flag indicating if the request being served is valid
    Reg#(Bool) is_dmem_req_valid <- mkRegU();



    // ------------------------------------------------------------------------
    // Data memory interface implementation
    // ------------------------------------------------------------------------

`ifdef RV32
    // Convert the data memory request size (e.g. HALFWORD) into byte enable 
    //signals
    function Vector#(BYTESWORD, Bool) size2ByteEnables(DMemSize size);
        case (size)
            DMEM_BYTE:          return unpack(4'b0001);
            DMEM_HALFWORD:      return unpack(4'b0011);
            DMEM_WORD:          return unpack(4'b1111);
            default:            return unpack(4'b0000);
        endcase
    endfunction
`elsif RV64
    function Vector#(BYTESWORD, Bool) size2ByteEnables(DMemSize size);
        case (size)
            DMEM_BYTE:          return unpack(8'b00000001);
            DMEM_HALFWORD:      return unpack(8'b00000011);
            DMEM_WORD:          return unpack(8'b00001111);
            DMEM_DOUBLEWORD:    return unpack(8'b11111111);
        endcase
    endfunction
`endif

    interface Put request;
        method Action put(DMemReq req);

            // Check if the data address is within memory
            if (req.addr + (1 << pack(req.size)) - 1 >= `DMEM_SIZE)
                is_dmem_req_valid <= False;

            else begin 
                // Store the request made so that the right BRAM banks are 
                // probed for response
                last_dmem_req <= req;

                // Decompose a data memory request into one or more byte accesses
                Vector#(BYTESWORD, Byte) req_vec = unpack(pack(req.write_data));
                Vector#(BYTESWORD, Bool) byte_enable = size2ByteEnables(req.size);
                
                for (Integer i = 0; i < bytes_in_word; i = i + 1) begin
                    // Pick the appropriate BRAM bank 
                    Addr byte_addr = req.addr + fromInteger(i);
                    ByteSel byte_offset = truncate(byte_addr);
                    dbram[byte_offset].portA.request.put(
                        BRAMRequest {
                            write:              req.mem_op == WRITE && byte_enable[i],
                            responseOnWrite:    True,
                            address:            truncate(byte_addr >> byte_sel_word),
                            datain:             req_vec[i]
                        });
                end
                is_dmem_req_valid <= True;
            end
        endmethod
    endinterface


    interface Get response;
         method ActionValue#(DMemResp) get();

            if (is_dmem_req_valid) begin
                // Combine responses from BRAM banks into a single word
                Vector#(BYTESWORD, Byte) resp_vec;
                Vector#(BYTESWORD, Bool) byte_enable = size2ByteEnables(
                    last_dmem_req.size);
                
                for (Integer i = 0; i < bytes_in_word; i = i + 1) begin
                    // Pick the appropriate BRAM bank 
                    Addr byte_addr = last_dmem_req.addr + fromInteger(i);
                    ByteSel byte_offset = truncate(byte_addr);
                    resp_vec[i] <- dbram[byte_offset].portA.response.get();
                end
                return tagged Mem_Resp_Ok unpack(pack(resp_vec));
            end
            else
                return tagged Mem_Resp_Exception EXC_DATA_ACCESS_FAULT;
        endmethod
    endinterface

endmodule

endpackage