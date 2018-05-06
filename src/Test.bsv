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
 * RISC-V spec tesbench.
 */


import RISCV_Spec::*;               // Custom libraries
import RISCV_Definitions::*;
import Definitions::*;
import Memory_BRAM::*;


`include "Config.bsv"               // Include the configuration options


typedef enum {START, TEST} Test_State deriving (Eq, Bits);


// ============================================================================
// Top-level test module
// ============================================================================
module mkTest(Empty);

    IMem_Ifc instr_memory <- mkIMem();

    // Instantiate memory
`ifdef DMEM_MULTIBANKED
    DMem_Ifc data_memory  <- mkDMemMultibanked(); 
`elsif DMEM_SINGLE_BRAM
    DMem_Ifc data_memory  <- mkDMemSimple(); 
`endif  

    // Instantiate the RISC-V specification
    RISCV_Ifc riscv <- mkRISCV_Spec(instr_memory, data_memory);

    // State used for testing   
    Reg#(Test_State) test_state <- mkReg(START);

    // Reset the model
    rule launch(test_state == START);
        riscv.reset();
        test_state <= TEST;
    endrule
    
endmodule
