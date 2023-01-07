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
//   this is only a demo to show how to use the sram, I didn't verfy it.
// ----------------------------------------------------

module hist_stat(
  input pclk,
  input rst_n,

  input enable_i,

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
  output ready_i,

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

  //register
  output [10:0] reg_addr_wr_o, 
  output [10:0] reg_addr_rd_o, 
  output  [7:0] reg_data_wr_o,
  input  [7:0]  reg_data_rd_i,


  // ----------------------------------------------------

);

// ----------------------------------------------------
// signle data handshake logic
// un-pipelined ready.
// valid->valid_q-> valid_o

// ----------------------------------------------------

wire valid_d1;
wire get_d0;
wire get_d1;


wire [7:0]  luma_fst;
wire [7:0]  luma_sec;
wire [8:0]  luma_sum;
wire [7:0]  luma_avg;

reg [7:0]  luma_avg_q;

reg  [11:0] counter_h;
reg  [11:0] counter_v;


valid_ready_simp_rs (
  .clk_i(pclk),
  .rst_n(rst_n),
);

assign get_d0 = valid_i & ready_i;
assign get_d1 = valid_d1 & ready_i;

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











endmodule



module valid_ready_simp_rs(
  input clk_i,
  input rst_n,

  input clr_i,
  input pre_rs_valid_i,
  input pre_rs_data_i,
  output rs_pre_ready_o,
  output rs_nxt_valid_o,
  output reg rs_nxt_data_o,
  input nxt_rs_ready_i
);



endmodule
