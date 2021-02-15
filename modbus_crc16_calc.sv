/*
	MIT License

	Copyright (c) 2021 Sergey Kuznetsov

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
*/



/*
	// Modbus CRC16 Calculator
	
	// Modbus checksum calculation module for 'data' array 
	// of 'size' up to 256 bytes.
	
	// Module crc_calculator is taken from
	// https://github.com/roman-pogorelov/verlib/tree/master/math
	
	// Other instances are taken from 
	// https://github.com/vasilydenisenko/hdl_primitives and
	// https://github.com/vasilydenisenko/fsm_kernels

	modbus_crc16_calc
		modbus_crc16_calc_inst
	(
		.clk	    (  ),
		.reset	    (  ),
                               
        .data	 	(  ), // i      [256 -1 : 0][7 : 0]
                               
	 	.start  	(  ), // i
        .size 		(  ), // i      [7 : 0]
    
        .crc_valid  (  ), // o
        .crc_data   (  )  // o      [15 : 0]
	);
*/



`default_nettype none
module modbus_crc16_calc
(	
	input  logic 					clk,
	input  logic 					reset,
	
	input  logic [MAX_SIZE -1 : 0]
				 [7 : 0]			data,
    
	input  logic 			        start,
	input  logic [7 : 0]        	size,
    
    output logic                    crc_valid,
	output logic [15 : 0]	        crc_data
);
    
    localparam int unsigned MAX_SIZE = 256;
    localparam DEFAULT_SIZE = '0;
    localparam CRC_POLYNOMIAL = 16'h8005; 
    localparam CRC_INITIAL_VAL = 16'hFFFF;
    localparam CRC_FINAL_XOR_VAL = 16'h0000; 
    localparam INPUT_REFLECTED = 1; 
    localparam RESULT_REFLECTED = 1;
    
    
	genvar i, j, k;
			
	logic [7 : 0] size_r;
	dff_ar
	#(
		.DWIDTH 	(8),
		.POR_VALUE 	(DEFAULT_SIZE)
	)
		size_r_inst
	(
		.clk		( clk ),
		.rst		( reset ),
		
		.in			( size ),	// i[DWIDTH - 1 : 0]
		.ena		( start && idle_st ),	// i
		.out		( size_r )	// o[DWIDTH - 1 : 0]
	);


	logic is_first_start;
	dff_ar
	#(
		.DWIDTH 	( 1 ),
		.POR_VALUE 	( 1'b1 )
	)
		is_first_start_inst
	(
		.clk		( clk ),
		.rst		( reset ),
		
		.in			( 1'b0 ),	// i[DWIDTH - 1 : 0]
		.ena		( start && idle_st ),	// i
		.out		( is_first_start )	// o[DWIDTH - 1 : 0]
	);


	logic [7 : 0] byte_count;
	counter_up_dw_ld
	#(
		.DWIDTH 		( 8 ),
		.DEFAULT_COUNT	( 0 )
	)
		byte_count_inst
	(
		.clk			( clk ),
		.rst			( reset ),		
		.cntrl__up_dwn	( 1'b1 ),	// i
		.cntrl__load	( idle_st ),	// i
		.cntrl__ena		( calc_and_count_st ),	// i						
		.cntrl__data_in	( 8'd0 ),	// i[DWIDTH - 1 : 0]		
		.count			( byte_count )	// o[DWIDTH - 1 : 0]
	);
	
	
	logic calc_done;
	assign calc_done = byte_count == size_r - 1'd1;


	logic idle_st, init_st, calc_and_count_st, final_xor_st;
	fsm_oe4s_sequencer
		work_states
	(
		.clk	( clk ),
		.rst	( reset ),
		
		.t01	( start ),	// i
		.t12	( 1'b1 ),	// i
		.t23	( calc_done ),	// i
		.t30	( 1'b1 ),	// i
		
		.st0	( idle_st ),	// o
		.st1	( init_st ),	// o
		.st2	( calc_and_count_st ),	// o
		.st3	( final_xor_st )	// o
	); 
		
		
	logic [MAX_SIZE -1 : 0][7 : 0] data_current;		
	generate
		
		for (j = 0; j < MAX_SIZE; j++)
		begin : reorders_input_outer_cycle
			for (i = 0; i < 8; i++) 
			begin : reorders_input_inner_cycle
				if(INPUT_REFLECTED)
					assign data_current[j][i] = data[j][7 - i];
				else
					assign data_current[j][i] = data[j][i];
			end : reorders_input_inner_cycle
		end : reorders_input_outer_cycle
		
	endgenerate
		
		
	logic [15 : 0] crc_value_next; 
	always_comb 
	begin
		if( init_st )
		begin
			crc_value_next = CRC_INITIAL_VAL;
		end else
		if ( calc_and_count_st )
		begin
			crc_value_next = crc_out;
		end else
		if ( final_xor_st )
		begin
			crc_value_next = crc_value ^ CRC_FINAL_XOR_VAL;
		end else
		begin
			crc_value_next = crc_value;
		end        
	end


	logic [7 : 0] crc_in; 
	assign crc_in = data_current[ byte_count ];


	logic [15 : 0] crc_value;
	dff_ar
	#(
		.DWIDTH 	( 16 ),
		.POR_VALUE 	( 16'h0000 )
	)
		crc_value_inst
	(
		.clk		( clk ),
		.rst		( reset ),
		
		.in			( crc_value_next ),	// i[DWIDTH - 1 : 0]
		.ena		( 1'b1 ),	// i
		.out		( crc_value )	// o[DWIDTH - 1 : 0]
	);
	
		
	//------------------------------------------------------------------------------------
	//      Модуль вычисления значения значения контрольной суммы CRC
	logic [15 : 0] crc_out; 
	crc_calculator
	#(
		.DATAWIDTH  ( 8 ), // Разрядность данных
		.CRCWIDTH   ( 16 ), // Разрядность CRC
		.POLYNOMIAL ( CRC_POLYNOMIAL )  // Порождающий полином
	)
	the_crc_calculator
	(
		// Входные данные
		.i_dat      ( crc_in ), // i  [DATAWIDTH - 1 : 0]
		
		// Входное (текущее) значение CRC
		.i_crc      ( crc_value ), // i  [CRCWIDTH - 1 : 0]
		
		// Выходное (расчитанное) значение CRC
		.o_crc      ( crc_out )  // o  [CRCWIDTH - 1 : 0]
	); // the_crc_calculator


	generate

		for (k = 0; k < 16; k++)
		begin : reorders_output_cycle
			if(RESULT_REFLECTED)
				assign crc_data[k] = crc_value[15 - k];
			else
				assign crc_data[k] = crc_value[k];
		end : reorders_output_cycle

	endgenerate


	assign crc_valid = idle_st & ~is_first_start;

endmodule : modbus_crc16_calc