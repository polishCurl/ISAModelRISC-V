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
 * Simple memory implementation. Harvard architecture - separate instruction and 
 * data memories.
 *
 * Data memory supports unaligned memory accesses but has multicycle latency.
 */


package Memory_BRAM;

import ClientServer::*;
import FIFO::*;
import GetPut::*;
import BRAM::*;
import Definitions::*;
import Vector::*;

`include "Config.bsv"


// ============================================================================
// Memory implementation
// ============================================================================
module mkMemory(Memory_Ifc);

    // ------------------------------------------------------------------------
    // Instruction memory controller
    // ------------------------------------------------------------------------
    // Configure and instantiate instruction BRAM
    BRAM_Configure ibram_cfg        = defaultValue;
    ibram_cfg.loadFormat            = tagged Hex `IMEM_FILE;
    ibram_cfg.memorySize            = valueOf(`IMEM_SIZE) / bytes_in_word;
    BRAM1Port#(Addr, Instr) ibram   <- mkBRAM1Server(ibram_cfg);

    // Instruction memory response buffer
    FIFO#(IMem_Resp) imem_resp_fifo <- mkFIFO;
    

    // ------------------------------------------------------------------------
    // Data memory controller
    // ------------------------------------------------------------------------
    // Configure and instantiate data BRAM. 
    BRAM_Configure dbram_cfg        = defaultValue;
    dbram_cfg.memorySize            = valueOf(`DMEM_SIZE);
    BRAM1Port#(Addr, Byte) dbram    <- mkBRAM1Server(dbram_cfg);

    // Data memory response buffer
    FIFO#(DMem_Resp) dmem_resp_fifo <- mkFIFO;

    // Byte-vector with data memory content to be send back to the requestor
    Reg#(Vector#(BYTEENLEN, Byte)) resp_vec <- mkRegU;

    // Currently processed data memory request
    Reg#(DMem_Req) current_req <- mkRegU;

    // Flag indicating if the data memory controller is ready to accept a new req        
    Reg#(Bool) busy <- mkReg(False);

    // Index of the data byte being loaded/stored
    Reg#(Bit#(TAdd#(BYTEENLEN,1))) byte_index <- mkRegU;




    // ------------------------------------------------------------------------
    // Instruction memory controller FSM
    // ------------------------------------------------------------------------
    rule imemRequest;
        Instr ibram_resp <- ibram.portA.response.get();
        imem_resp_fifo.enq(tagged Mem_Resp_Ok ibram_resp);
    endrule



    // ------------------------------------------------------------------------
    // Data memory controller FSM
    // ------------------------------------------------------------------------
    // A data memory request is being served
    rule dmemRequestProcess (busy && byte_index < (1 << pack(current_req.size)));

        // Read the response for the last BRAM request sent
        Byte dbram_resp  <- dbram.portA.response.get();
        resp_vec[byte_index - 1] <= dbram_resp;

        // Issue the next load/store byte request to BRAM
        Addr byte_addr = current_req.addr + zeroExtend(byte_index);
        Vector#(BYTEENLEN, Byte) data_vec = unpack(pack(current_req.write_data));
        
        if (byte_addr >= `DMEM_SIZE)
            dmem_resp_fifo.enq(tagged Mem_Resp_Exception EXC_DATA_ACCESS_FAULT);
        else
            dbram.portA.request.put(
                    BRAMRequest {
                        write:              current_req.mem_op == DMEM_WRITE,
                        responseOnWrite:    True,
                        address:            byte_addr,
                        datain:             data_vec[byte_index]
                    });

        byte_index <= byte_index + 1;
    endrule


    // Complete the data memory response
    rule dmemRequestFinish (busy && byte_index >= (1 << pack(current_req.size)));

        // Read the final byte from data BRAM and form the response
        Vector#(BYTEENLEN, Byte) final_resp_vec = resp_vec;
        final_resp_vec[byte_index - 1] <- dbram.portA.response.get();
        busy <= False;
        dmem_resp_fifo.enq(tagged Mem_Resp_Ok unpack(pack(final_resp_vec)));
    endrule



    // ------------------------------------------------------------------------
    // Instruction memory interface implementation
    // ------------------------------------------------------------------------
    interface Server imem;

        interface Put request;
            method Action put(Addr addr);

                // Check if the instruction address is aligned and within memory
                if (addr[1:0] != 0 || addr >= `IMEM_SIZE)
                    imem_resp_fifo.enq(tagged Mem_Resp_Exception EXC_INSTR_ACCESS_FAULT);
                else 
                    ibram.portA.request.put(BRAMRequest {
                        write :             False,
                        responseOnWrite :   False,
                        address :           addr >> byte_sel_len,
                        datain :            ?
                    });
            endmethod
        endinterface

        interface response = toGet(imem_resp_fifo);
    endinterface


    // ------------------------------------------------------------------------
    // Data memory interface implementation
    // ------------------------------------------------------------------------
    interface Server dmem;

        // Split the incoming data memory request into one or more BRAM requests
        // processed sequentially
        interface Put request;
            method Action put(DMem_Req req) if (!busy);

                // Convert input data into a vector of bytes
                Vector#(BYTEENLEN, Byte) data_vec = unpack(pack(req.write_data));

                // Check if the byte address is valid and send the first data 
                // BRAM request
                if (req.addr >= `DMEM_SIZE)
                    dmem_resp_fifo.enq(tagged Mem_Resp_Exception EXC_DATA_ACCESS_FAULT);
                else
                    dbram.portA.request.put(
                            BRAMRequest {
                                write:              req.mem_op == DMEM_WRITE,
                                responseOnWrite:    True,
                                address:            req.addr,
                                datain:             data_vec[0]
                            });

                // Initialise the memory controller state
                current_req     <= req;
                busy            <= True;
                byte_index      <= 1;
            endmethod
        endinterface


        interface response = toGet(dmem_resp_fifo);
    endinterface

endmodule

endpackage