//  ******************************************************
//  Author            : Wenxuan Hu
//  Last modified     : 2023-01-06 02:26
//  Email             : huwe0427@uw.edu
//  Student Number    : 2027482
//  Filename          : hist_stat.v
//  Description       :
//    Copyright [2023] <Copyright Wenxuan Hu>
//  ******************************************************

// Jian Liang's version
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
  //address 7 bits for 1 block; max 16 blocks -> 11 bits; 128 level
  output [10:0] addr_wr_o, 
  output [10:0] addr_rd_o, 
  output wr_wen_o,
  output rd_wen_o,
  output wr_cen_o,
  output rd_cen_o,
  output  [7:0] data_wr_o,
  input  [7:0] data_rd_i,

  //register file
  output [10:0] reg_addr_wr_o, 
  output [10:0] reg_addr_rd_o, 
  output  [7:0] reg_data_wr_o,
  input  [7:0]  reg_data_rd_i,
  output  reg_enable,

  //memory ping-pang toggle
  output mem_toggle 
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

//memory ctrl
wire [7:0]  pre_counter_lo;
wire [7:0]  counter_nxt_lo;

wire [7:0]  pre_counter_hi;
wire [7:0]  counter_nxt_hi;

reg   [10:0]  addr_rd_q; // sram wr addr;

// one bit for carry logic
wire carry;   
reg carry_q;


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
// toggle for memory; dont need to delay
// pixel line:  line blanking time-----> pixel valid time ----> line blanking time
assign mem_toggle = ~counter_v[8];




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

assign reg_addr_rd_o  = {cur_block, luma_avg[7:1]};
assign pre_counter_lo  = reg_data_rd_i;

assign {carry, counter_nxt_lo}  = pre_counter_lo + 1'b1;
assign reg_addr_wr_o  = {cur_block, luma_avg[7:1]};
assign reg_data_wr_o  = counter_nxt_lo;

assign reg_enable  = get_d1;

// ----------------------------------------------------
// update the high-bits parts -> SRAM
always@(posedge pclk or negedge rst_n)begin
  if(~rst_n)
    carry_q <=  1'b0;
  else 
    carry_q <=  carry;
end

assign rd_wen_o = carry;
assign rd_cen_o = ~carry;
assign wr_wen_o = ~carry_q;
assign wr_cen_o = ~carry_q;

assign addr_rd_o  = carry?  {cur_block, luma_avg[7:1]} : 'd0;
assign addr_wr_o  = addr_rd_q;

always@(posedge pclk or negedge rst_n)  begin
    if(~rst_n) 
      addr_rd_q <=  'd0;
    else if(carry)
      addr_rd_q <=  {cur_block, luma_avg[7:1]};
end


assign pre_counter_hi = data_rd_i;
assign counter_nxt_hi = pre_counter_hi  + 1'b1;

assign data_wr_o  = counter_nxt_hi;


// memory will be reset/clear by curve_calc module


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

wire cen_rd_0 = rd_cen_o |  toggle;
wire cen_rd_1 = rd_cen_o |  toggle;

wire cen_wr_0 = wr_cen_o |  toggle;
wire cen_wr_1 = wr_cen_o |  toggle;

wire wen_wr_0 = wr_wen_o |  toggle;
wire wen_wr_1 = wr_wen_o |  toggle;


wire [7:0]  data_rd_0;
wire [7:0]  data_rd_1;

wire [7:0]  data_rd;
assign data_rd = toggle ? data_rd_1 : data_rd_0;

// memory can be read by hist module and curve_calc module
// memory can be write by hist module, reset by curve_calc

// 1w1r sram
sram_1r1w inst_0(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_0 & curve_calc_read_logic),
  .cen_wr(cen_wr_0),
  .wen_wr(wen_wr_0),

  .addr_rd(addr_rd   & curve_calc_read_addr),
  .data_rd(data_rd_0),
  .addr_wr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);

sram_1r1w inst_1(
  .clk(pclk),
  .rst_n(rst_n),
  
  .cen_rd(cen_rd_1 & curve_calc_read_logic),
  .cen_wr(cen_wr_1),
  .wen_wr(wen_wr_1),

  .addr_rd(addr_rd   & curve_calc_read_addr),
  .data_rd(data_rd_1),
  .addr_wr(addr_wr   & curve_calc_write_addr),
  .data_wr(data_wr)
);


wire [7:0]  reg_data_rd_0;
wire [7:0]  reg_data_rd_1;

wire [7:0]  reg_data_rd;
assign reg_data_rd = toggle ? reg_data_rd_1 : reg_data_rd_0;
//regfile
regfile inst_2(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_0)

  .enanle(~toggle & reg_enable |  curve_calcu_enable_logic)
);

regfile inst_3(
  .cll(pclk),
  .rst_n(rst_n),

  .addr_wr(reg_addr_wr),
  .addr_rd(reg_addr_rd),
  .data_wr(reg_data_wr),
  .data_rd(reg_data_rd_1)

  .enanle(toggle & reg_enable |  curve_calcu_enable_logic)
);

endmodule
