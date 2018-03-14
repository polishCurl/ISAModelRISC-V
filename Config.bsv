// ============================================================================
// RISC-V ISA model configuration options. (Comment out to disable)
// ============================================================================

`define RV32M					// Integer Multiplication and Division 
`define TRACE_EXECUTION 		// Print instruction execution debug messages







// Derive any additional MACROS given the configuration
`ifdef RV64I
	`ifdef RV32M
		`define RV64M
	`endif
`endif	

