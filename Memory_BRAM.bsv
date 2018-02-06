package Memory_BRAM;

import ClientServer::*;
import BRAM::*;
import Definitions::*;




// ============================================================================
// Memory implementation
// ============================================================================
module mkMemory#(Integer instr_mem_size,		// Instruction memoru size
				 Integer data_mem_size,			// Data memory size
				 String prog_src_filename)		// Source file with test program
				(Memory_Ifc);

	// Congifure and instantiate instruction BRAM
	BRAM_Configure ibram_cfg = defaultValue;
    ibram_cfg.loadFormat = tagged Hex prog_src_filename;
	BRAM1Port#(Word, Word) ibram <- mkBRAM1Server(ibram_cfg);


	// ----------------------------------------------------------------------------
	// Instruction memory
	// ----------------------------------------------------------------------------
  	interface Server imem;

  	  	// Request instruction
  	  	interface Put request;
  			method Action put(Word addr);
  				ibram.portA.request.put(BRAMRequest {
					write : 			False,
					responseOnWrite : 	False,
					address : 			addr >> 2,
					datain : 			0});
  			endmethod
  		endinterface

  		// Receive instruction
  		interface Get response;
            method ActionValue#(Word) get();
            	let result <- ibram.portA.response.get();
            	return result;
            endmethod
        endinterface

  	endinterface




  	/*
  	BRAM_Configure dbram_cfg = BRAM_Configure {
		memorySize : 		dmem_size,
		latency : 			1,
		outFIFODepth : 		3,
		loadFormat : 		None,
		allowWriteResponseBypass : False
	}; 
 

	BRAM1PortBE#(PCT, WordT) dbram <- mkBRAM1ServerBE(dbram_cfg);


  	interface Server dmem;

  		interface Put#(DMem_Req) request;
  			method Action put(Word req);


  			endmethod
  		endinterface


  		interface Get#(Word) response;
  			 method ActionValue#(Word) get();


  			 endmethod
        endinterface

  	endinterface
  	*/

endmodule



endpackage