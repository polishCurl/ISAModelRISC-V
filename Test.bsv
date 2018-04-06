import Definitions::*;
import RISCV_Spec::*;
import Memory_BRAM::*;
import ClientServer::*;
import GetPut::*;
import BRAM::*;


typedef enum {START, TEST} Test_State deriving (Eq, Bits);


// ============================================================================
// Top-level test module
// ============================================================================
module mkInstr_Test(Empty);

    // DUTs
    Memory_Ifc mem <- mkMemory();   
    RISCV_Ifc riscv <- mkRISCV_Spec(mem);

    // State used for testing   
    Reg#(Test_State) test_state <- mkReg(START);

    // Reset the model
    rule launch(test_state == START);
        riscv.reset();
        test_state <= TEST;
    endrule
    
endmodule
