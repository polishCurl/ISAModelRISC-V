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
// Register width and address space size
// ----------------------------------------------------------------------------
//`define RV64 						// Use 64-bit version instead of 32-bit			



// ----------------------------------------------------------------------------
// ISA extensions
// ----------------------------------------------------------------------------
`define RVxxM						// Integer Multiplication and Division 


// ----------------------------------------------------------------------------
// Input files with memory contents
// ----------------------------------------------------------------------------
`define IMEM_SIZE 1024				// Instruction memory size (in bytes)
`define DMEM_SIZE 1024				// Data memory size (in bytes)
`define IMEM_FILE "imem.hex" 		// Input file with instruction memory contents
`define DMEM_FILE "dmem.hex" 		// Input file with data memory contents
	

// ----------------------------------------------------------------------------
// Other settings
// ----------------------------------------------------------------------------
`define TRACE_EXECUTION 			// Print debug messages



