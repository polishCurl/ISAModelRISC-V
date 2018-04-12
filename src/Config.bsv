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
 * RISC-V ISA model configuration file.
 */



// ============================================================================
// RISC-V ISA model configuration options. 
// ============================================================================

// ----------------------------------------------------------------------------
// Register width
// ----------------------------------------------------------------------------
`define RV32                        // Use a 32-bit version of the architecture       
//`define RV64                      // Use a 64-bit version of the architecture       



// ----------------------------------------------------------------------------
// ISA extensions
// ----------------------------------------------------------------------------
`define RVxxM                       // Integer Multiplication and Division 


// ----------------------------------------------------------------------------
// Provide external visibility to RISC-V model state and control over the 
// operation of the synthesized spec
// ----------------------------------------------------------------------------
`define DEBUG_ACCESS                // Allow debug access


// ----------------------------------------------------------------------------
// Input files with memory contents
// ----------------------------------------------------------------------------
// Filename with memory contents
`define MEM_FILE "sltiu.hex"

// Path to the instruction memory file
//`define IMEM_PATH "../test/rv32/isa/imem/" + `MEM_FILE
`define IMEM_PATH "../csr.hex"

// Path to the single BRAM data memory file  
`define DMEM_PATH "../test/rv32/isa/dmem/" + `MEM_FILE  

// Path to the multibanked data memory files. The appended indexes indicate the 
// bank (e.g. ../test/rv32/isa/dmemMulti/lh.hex2 for the third data memory bank)     
`define DMEM_MULTI_PATH "../test/rv32/isa/dmemMulti/" + `MEM_FILE  


// ----------------------------------------------------------------------------    
// Memory sizes in bytes (must be a powers of 2)
// ----------------------------------------------------------------------------
`define IMEM_SIZE 16384             // Instruction memory 
`define DMEM_SIZE 16384             // Data memory


// ----------------------------------------------------------------------------    
// Type of data memory used (choose one)
// ----------------------------------------------------------------------------
//`define DMEM_MULTIBANKED            // Use multibanked memory with one-cycle latency
`define DMEM_SINGLE_BRAM          // Use a single-BRAM implementation with variable
                                    // latency

// ----------------------------------------------------------------------------
// PC reset value
// ----------------------------------------------------------------------------
`define PC_RESET_VAL 0              // Address of the first instruction to fetch 
                                    // after reset

// ----------------------------------------------------------------------------
// Other settings
// ----------------------------------------------------------------------------
`define TRACE_EXECUTION             // Print debug messages

