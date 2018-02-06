package Register_Files;

import RegFile:: *;
import Definitions::*;


// ----------------------------------------------------------------------------
// General Purpose Registers (GPR)
// ----------------------------------------------------------------------------
GPR x0  =  0;    GPR x1  =  1;    GPR x2  =  2;    GPR x3  =  3;
GPR x4  =  4;    GPR x5  =  5;    GPR x6  =  6;    GPR x7  =  7;
GPR x8  =  8;    GPR x9  =  9;    GPR x10 = 10;    GPR x11 = 11;
GPR x12 = 12;    GPR x13 = 13;    GPR x14 = 14;    GPR x15 = 15;
GPR x16 = 16;    GPR x17 = 17;    GPR x18 = 18;    GPR x19 = 19;
GPR x20 = 20;    GPR x21 = 21;    GPR x22 = 22;    GPR x23 = 23;
GPR x24 = 24;    GPR x25 = 25;    GPR x26 = 26;    GPR x27 = 27;
GPR x28 = 28;    GPR x29 = 29;    GPR x30 = 30;    GPR x31 = 31;


// ----------------------------------------------------------------------------
// General Purpose Registers Interface
// ----------------------------------------------------------------------------
interface GPR_Ifc;
    method Action write(GPR r, Word data);
    method Word read(GPR r);
endinterface


module mkGPR(GPR_Ifc);

    // Instantiate GPR register file
    RegFile#(GPR, Word) gpr <- mkRegFileWCF(1, fromInteger(x_num-1));

    // Write to GPR
    method Action write(GPR r, Word data);
        trace($format("Write: x%d:=0x%h", r, data));
        if (r != x0)
            gpr.upd(r, data);
    endmethod

    // Read GPR
    method Word read(GPR r);
        return r == x0 ? 0 : gpr.sub(r);
    endmethod

endmodule


// ----------------------------------------------------------------------------
// Program Counter Interface
// ----------------------------------------------------------------------------
interface PC_Ifc;
    method Action write(Word data);
    method Word read();
endinterface


module mkPC(PC_Ifc);

    // Instantiate Program Counter
    Reg#(Word) pc <- mkRegU;            

    // Write to PC
    method Action write(Word data);
        trace($format("Write: pc:=0x%h", data));
        pc <= data;
    endmethod

    // Read PC
    method Word read();
        return pc;
    endmethod

endmodule



endpackage
