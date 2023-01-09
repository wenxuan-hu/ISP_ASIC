//  ******************************************************
//  Author            : Wenxuan Hu
//  Last modified     : 2023-01-06 02:35
//  Email             : huwe0427@uw.edu
//  Student Number    : 2027482
//  Filename          : hist_stat.v
//  Description       :
//    Copyright [2023] <Copyright Wenxuan Hu>
//  ******************************************************

//  another version for signle port SRAM and regfile
// ----------------------------------------------------
//   2 pixel per cycle  : horizon direction
//   sub-sample: average
//   histogram: 128 level
//   image 2560x1920
//   block size 256x256
//   block num  10x8
//   
//
//   this is only a demo to show how to use the sram, I didn't verfy it. //TODO
// ----------------------------------------------------

module hist_stat(
  input pclk,
  input rst_n,

  input enable_i,

  // pixel data  rgb
  input [7:0] pixel_fst_r_i,
  input [7:0] pixel_sec_r_i,
  input [7:0] pixel_fst_g_i,
  input [7:0] pixel_sec_g_i,
  input [7:0] pixel_fst_b_i,
  input [7:0] pixel_sec_b_i,

  //size
  input [11:0] frame_width_i,
  input [10:0] frame_height_i,
  input [9:0] block_width_i,
  input [9:0] block_height_i,

  input [4:0] block_num_h_i,
  input [3:0] block_num_v_i,

  // range of interest size/offset
  //input [11:0] roi_width,
  //input [10:0] roi_height,
  //input [11:0] roi_hor,
  //input [10:0] roi_ver,


// horizon and vertical sync
  input h_sync_i,
  input v_sync_i,

  // end of frame -> clear -> start of frame; from top controller
  input eof_i,
  input clr_i,
  input sof_i,  



  input ready_i,
  input valid_i,


  output reg valid_o,
  output ready_o,

  // ----------------------------------------------------
  //mem 
  //sram //TODO 
  // 128 level table, 8 bits per entry
  //
  // 4 SRAMs in total, 2 per block line,  sram width 16 bits:  8*64*16bits
  // addr: 3+6 
  output [8:0] addr_o, 
  output  wen,
  output  cen,
  output  [15:0] data_wr_o,
  input  [15:0] data_rd_i,
  output sram_toggle,
  //register file
  //  4 regfiles, 2 per block line
  //  8*128 level * 8bits
  output [9:0] reg_addr_wr_o, 
  output [9:0] reg_addr_rd_o, 
  output  [7:0] reg_data_wr_o,
  input  [7:0]  reg_data_rd_i,
  output  reg_enable,
  output reg_toggle, // for adjecent block

  //memory ping-pang toggle
  output mem_toggle  //for block line 
  // ----------------------------------------------------

);

//clk gating logic

// ----------------------------------------------------
// signle data handshake logic
// un-pipelined ready.
// valid->valid_q-> valid_o

// ----------------------------------------------------

reg valid_d1;
reg valid_d2;
wire get_d0;
wire get_d1;
wire get_d2;
wire get_d3;


wire [7:0]  luma_fst;
wire [7:0]  luma_sec;
wire [8:0]  luma_sum;
wire [7:0]  luma_avg;

reg [7:0]  luma_avg_q;

reg  [11:0] counter_h;
reg  [11:0] counter_v;

wire  [11:0] counter_h_nxt;
wire  [11:0] counter_v_nxt;


// current block location
wire [3:0] cur_block;
wire [3:0] pre_block;

//memory ctrl
wire [7:0]  pre_counter_lo;
wire [7:0]  counter_nxt_lo;

wire [15:0]  pre_counter_hi;
wire [15:0]  counter_nxt_hi;

reg   [8:0]  addr_rd_q; // sram wr addr;

wire  sram_rd;
reg  sram_wr;


// one bit for carry logic
wire carry;   
reg carry_q;

reg [127:0] carry_mem_a;
reg [127:0] carry_mem_b;

wire [1:0]  carry_clear;
wire [6:0] carry_counter; // counting for move the carry bit to sram.

// ----------------------------------------------------
// hand shake
//valid_ready_simp_rs  hs_stage_0 (
//  .clk_i(pclk),
//  .rst_n(rst_n),
//);

assign get_d0 = valid_i  & ready_i;
assign get_d1 = valid_d1 & ready_i;
assign get_d2 = valid_d2 & ready_i;
assign get_d2 = valid_o & ready_i;

always  @(posedge pclk or negedge rst_n)  begin
  if (~rst_n)  
    valid_d1  <=  1'b1;
  else if(ready_i)
    valid_d1  <=  valid_i;
end
always  @(posedge pclk or negedge rst_n)  begin
  if (~rst_n)  
    valid_d2  <=  1'b1;
  else if(ready_i)
    valid_d2  <=  valid_d1;
end
always  @(posedge pclk or negedge rst_n)  begin
  if (~rst_n)  
    valid_o  <=  1'b1;
  else if(ready_i)
    valid_o  <=  valid_d2;
end
assign ready_o  = ready_i;  


// ----------------------------------------------------
// timing control
// TODO
// this logic only  suit to 2^x block size. Just simplify the design
always @(posedge pclk or negedge rst_n) begin
  if(~rst_n)
    counter_h <=  'd0;
  else if(get_d1)
    counter_h <=  counter_h_nxt;
end

assign counter_h_nxt  = (counter_h == (frame_width_i-'d1)) ? 'd0 : counter_h + 'd1;

always @(posedge pclk or negedge rst_n) begin
  if(~rst_n)
    counter_v <=  'd0;
  else if(get_d1 & counter_h == (frame_width_i-'d1))
    counter_v <=  counter_v_nxt;
end

assign counter_v_nxt  = (counter_h == (frame_width_i-'d1)) ?   (  (counter_v  ==  (frame_height_i - 'd1)) ? 'd0 : counter_v + 'd1 ) : counter_v;




assign cur_block = counter_h[11:8];
assign pre_block = cur_block=='d0  ?  ( counter_v < frame_width ? 'd0  : block_num_h_i )  : cur_block  - 1'b1 ;
// toggle for memory; dont need to delay
// pixel line:  line blanking time-----> pixel valid time ----> line blanking time
assign mem_toggle = counter_v[8];




// calu aeverage pixel value, sub sample
assign luma_fst = (pixel_fst_r_i > pixel_fst_g_i) ? ((pixel_fst_r_i > pixel_fst_b_i)?pixel_fst_r_i : pixel_fst_b_i) :((pixel_fst_g_i > pixel_fst_b_i)?pixel_fst_g_i : pixel_fst_b_i);
assign luma_sec = (pixel_sec_r_i > pixel_sec_g_i) ? ((pixel_sec_r_i > pixel_sec_b_i)?pixel_sec_r_i : pixel_sec_b_i) :((pixel_sec_g_i > pixel_sec_b_i)?pixel_sec_g_i : pixel_sec_b_i);

assign luma_sum = luma_fst + luma_sec + 1'b1;
assign luma_avg = luma_sum[8:1];

always@(posedge pclk or negedge rst_n)begin
  if(~rst_n)
    luma_avg_q  <=  'd0;
  else if(get_d0)  //for gate
    luma_avg_q  <=  luma_avg;
end


// ----------------------------------------------------
// update low-bits parts-> reg file

assign reg_addr_rd_o  = {cur_block[3:1], luma_avg[7:1]};
assign pre_counter_lo  = reg_data_rd_i;

assign {carry, counter_nxt_lo}  = pre_counter_lo + 1'b1;
assign reg_addr_wr_o  = {cur_block[3:1], luma_avg[7:1]};
assign reg_data_wr_o  = counter_nxt_lo;

assign reg_enable  = get_d1;

assign reg_toggle = cur_block[0]; 


// ----------------------------------------------------
// update the high-bits parts -> SRAM
always@(posedge pclk or negedge rst_n)  begin
  if (~rst_n)
    dummy_counter <=  'd0;
  else if(dummy_counter == 'd127)
    dummy_counter <=  'd0;
  else if(dummy_counter == 'd1)
    dummy_counter <=  dummy_counter + 1'b1;
  else if(pre_block == block_num_h_i  & counter_h[6:0] == 'd0)
    dummy_counter <=  dummy_counter + 1'b1;
end


assign carry_counter = pre_counter == block_num_h_i  ?dummy_counter  : counter_h[6:0];

always @(posedge pclk or negedge rst_n) begin
  if (~rst_n)
    carry_mem_a <=  128'd0;
  else if (~reg_toggle & reg_enable & carry)
    carry_mem_a[luma[7:1]]  <=  1'b1;
  else if (reg_toggle & carry_clear[0])
    carry_mem_a[{carry_counter[6:1], 1'b0}]  <=  1'b0;
  else if (reg_toggle & carry_clear[1])
    carry_mem_a[{carry_counter[6:1], 1'b1}]  <=  1'b0;
end
always @(posedge pclk or negedge rst_n) begin
  if (~rst_n)
    carry_mem_b <=  128'd0;
  else if (reg_toggle & reg_enable & carry)
    carry_mem_b[luma[7:1]]  <=  1'b1;
  else if (~reg_toggle & carry_clear[0])
    carry_mem_b[{carry_counter[6:1], 1'b0}]  <=  1'b0;
  else if (~reg_toggle & carry_clear[1])
    carry_mem_b[{carry_counter[6:1], 1'b1}]  <=  1'b0;
end





assign wen  = ~sram_wr;
assign cen  = ~(sram_wr|sram_rd);

assign carry_clear[1] = sram_toggle  ? carry_mem_a[{carry_counter[6:1], 1'b1}] : carry_mem_b[{carry_counter[6:1], 1'b1}];
assign carry_clear[0] = sram_toggle  ? carry_mem_a[{carry_counter[6:1], 1'b0}] : carry_mem_b[{carry_counter[6:1], 1'b0}];


assign sram_rd  = | carry_clear;
always @(posedge pclk or negedge rst_n)begin
  if(~rst_n)
    sram_wr <=  1'b0;
  else 
    sram_wr <=  sram_rd;
end

assign addr_o = {pre_block , carry_counter[6:1]};

always@(posedge pclk or negedge rst_n)  begin
    if(~rst_n) 
      addr_rd_q <=  'd0;
    else if(|carry_clear)
      addr_rd_q <=  {pre_block[3:1], carry_counter[6:1]};
end


assign pre_counter_hi = data_rd_i;
assign counter_nxt_hi[7:0] = pre_counter_hi[7:0]  + 1'b1 : pre_counter_hi[7:0];
assign counter_nxt_hi[15:8] = pre_counter_hi[15:8]  + 1'b1 : pre_counter_hi[15:8];

assign data_wr_o  = counter_nxt_hi;


// memory will be reset/clear by curve_calc module
assign sram_toggle  = pre_block[0];




endmodule


// ----------------------------------------------------
// back pressure
module valid_ready_simp_rs(
  input clk,
  input rst_n,

  input clr_i,
  input pre_rs_valid_i,
  input pre_rs_data_i,
  output rs_pre_ready_o,
  output rs_nxt_valid_o,
  output reg rs_nxt_data_o,
  input nxt_rs_ready_i
);

reg full;
always @(posedge clk or negedge rst_n)  begin
  if (~rst_n)
    full  <=  1'b0;
  else if (clr_i)
    full  <=  1'b0;
  else if(~pre_rs_valid_i  & nxt_rs_ready_i )
    full  <=  1'b0;
  else if(pre_rs_valid_i  & ~nxt_rs_ready_i )
    full  <=  1'b1;
end

  assign rs_nxt_valid_o = full;

always @(posedge clk oe negedge rst_n) begin
  if( ~rst_n) 
    rst_nxt_data_o  <= 'd0;
  else if (clr_i)
    rst_nxt_data_o  <= 'd0;
  else if(pre_rs_valid_i & (~full |nxt_rs_ready_i))
    rst_nxt_data_o  <=  pre_rs_data_i;
end

  assign rs_pre_ready_o = nxt_rs_ready_i | ~full;
  assign rs_nxt_valid_o = full;

endmodule



// ----------------------------------------------------
// memory usage in top level

module acad_top();

wire cen_0_a = cen_o_a |  toggle;
wire cen_1_a = cen_o_a |  toggle;
wire cen_0_b = cen_o_b |  toggle;
wire cen_1_b = cen_o_b |  toggle;


wire wen_0_b = wen_o_b |  toggle;
wire wen_1_b = wen_o_b |  toggle;


wire [15:0]  data_rd_0_a;
wire [15:0]  data_rd_0_b;
wire [15:0]  data_rd_1_a;
wire [15:0]  data_rd_1_b;

wire [15:0]  data_rd_a;
wire [15:0]  data_rd_b;
assign data_rd_a = toggle ? data_rd_1_a : data_rd_0_a;
assign data_rd_b = toggle ? data_rd_1_b : data_rd_0_b;

// memory can be read by hist module and curve_calc module
// memory can be write by hist module, reset by curve_calc

// 1w1r sram
sram_1r1w inst_0_a(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_0_a & curve_calc_read_logic),
  .wen_wr(wen_wr_0_a),

  .data_rd(data_rd_0_a),
  .addr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);
sram_1r1w inst_1_a(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_1_a & curve_calc_read_logic),
  .wen_wr(wen_wr_1_a),

  .data_rd(data_rd_1_a),
  .addr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);
sram_1r1w inst_0_b(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_0_b & curve_calc_read_logic),
  .wen_wr(wen_wr_0_b),

  .data_rd(data_rd_0_b),
  .addr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);
sram_1r1w inst_1_b(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_1_b & curve_calc_read_logic),
  .wen_wr(wen_wr_1_b),

  .data_rd(data_rd_1_b),
  .addr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);

// ----------------------------------------------------
// reg

wire [7:0]  reg_data_rd_0_a;
wire [7:0]  reg_data_rd_1_a;

wire [7:0]  reg_data_rd_a;
assign reg_data_rd_a = toggle ? reg_data_rd_1_a : reg_data_rd_0_a;
//regfile
regfile inst_2_a(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_0_a)

  .enanle(~toggle & ~reg_toggle & reg_enable |  curve_calcu_enable_logic)
);

regfile inst_3_a(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_1_a)

  .enanle(toggle & reg_toggle& reg_enable |  curve_calcu_enable_logic)
);



wire [7:0]  reg_data_rd_0_b;
wire [7:0]  reg_data_rd_1_b;

wire [7:0]  reg_data_rd_b;
assign reg_data_rd_b = toggle ? reg_data_rd_1_b : reg_data_rd_0_b;
//regfile
regfile inst_2_b(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_0_b)

  .enanle(~toggle & reg_toggle& reg_enable |  curve_calcu_enable_logic)
);

regfile inst_3_b(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_1_b)

  .enanle(toggle & reg_toggle & reg_enable |  curve_calcu_enable_logic)
);


endmodule
