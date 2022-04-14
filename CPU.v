module CPU(
  input         clock,
  input         reset,
  output [15:0] ddram_a,
  output [2:0]  ddram_ba,
  output        ddram_ras_n,
  output        ddram_cas_n,
  output        ddram_we_n,
  output        ddram_cs_n,
  output [1:0]  ddram_dm,
  input  [15:0] ddram_dq_in,
  output [15:0] ddram_dq_out,
  input  [1:0]  ddram_dqs_p_in,
  output [1:0]  ddram_dqs_p_out,
  output        ddram_clk_p,
  output        ddram_cke,
  output        ddram_reset_n,
  output        rgb_led0_r,
  output        rgb_led0_g,
  output        rgb_led0_b,
  input         usr_btn,
  input         usb_d_p_in,
  output        usb_d_p_out,
  input         usb_d_n_in,
  output        usb_d_n_out,
  input         usb_pullup_in,
  output        usb_pullup_out,
  output        spiflash4x_cs_n,
  input  [3:0]  spiflash4x_dq_in,
  output [3:0]  spiflash4x_dq_out
);
  assign ddram_a = 16'h0;
  assign ddram_ba = 3'h0;
  assign ddram_ras_n = 1'h0;
  assign ddram_cas_n = 1'h0;
  assign ddram_we_n = 1'h0;
  assign ddram_cs_n = 1'h0;
  assign ddram_dm = 2'h0;
  assign ddram_dq_out = 16'h0;
  assign ddram_dqs_p_out = 2'h0;
  assign ddram_clk_p = clock; // @[CPU.scala 56:15]
  assign ddram_cke = 1'h0; // @[CPU.scala 57:13]
  assign ddram_reset_n = 1'h1; // @[CPU.scala 58:17]
  assign rgb_led0_r = 1'h0; // @[CPU.scala 64:14]
  assign rgb_led0_g = 1'h0; // @[CPU.scala 65:14]
  assign rgb_led0_b = 1'h1; // @[CPU.scala 66:14]
  assign usb_d_p_out = 1'h0;
  assign usb_d_n_out = 1'h0;
  assign usb_pullup_out = 1'h0;
  assign spiflash4x_cs_n = 1'h0;
  assign spiflash4x_dq_out = 4'h0;
endmodule
