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
  output        rst_n,
  output        spiflash4x_cs_n,
  input  [3:0]  spiflash4x_dq_in,
  output [3:0]  spiflash4x_dq_out
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
`endif // RANDOMIZE_REG_INIT
  reg  uR; // @[CPU.scala 67:19]
  reg  uG; // @[CPU.scala 68:19]
  reg  uB; // @[CPU.scala 69:19]
  wire  _GEN_2 = usr_btn ? uG : uB; // @[CPU.scala 71:18 69:19 74:8]
  assign ddram_a = 16'h0;
  assign ddram_ba = 3'h0;
  assign ddram_ras_n = 1'h0;
  assign ddram_cas_n = 1'h0;
  assign ddram_we_n = 1'h0;
  assign ddram_cs_n = 1'h0;
  assign ddram_dm = 2'h0;
  assign ddram_dq_out = 16'h0;
  assign ddram_dqs_p_out = 2'h0;
  assign ddram_clk_p = clock; // @[CPU.scala 59:15]
  assign ddram_cke = 1'h0; // @[CPU.scala 60:13]
  assign ddram_reset_n = 1'h1; // @[CPU.scala 61:17]
  assign rgb_led0_r = uR; // @[CPU.scala 77:14]
  assign rgb_led0_g = uG; // @[CPU.scala 78:14]
  assign rgb_led0_b = uB; // @[CPU.scala 79:14]
  assign usb_d_p_out = 1'h0;
  assign usb_d_n_out = 1'h0;
  assign usb_pullup_out = 1'h0;
  assign rst_n = 1'h0; // @[CPU.scala 81:9]
  assign spiflash4x_cs_n = 1'h0;
  assign spiflash4x_dq_out = 4'h0;
  always @(posedge clock) begin
    if (reset) begin // @[CPU.scala 67:19]
      uR <= 1'h0; // @[CPU.scala 67:19]
    end else if (usr_btn) begin // @[CPU.scala 71:18]
      uR <= uB; // @[CPU.scala 72:8]
    end
    if (reset) begin // @[CPU.scala 68:19]
      uG <= 1'h0; // @[CPU.scala 68:19]
    end else if (usr_btn) begin // @[CPU.scala 71:18]
      uG <= uR; // @[CPU.scala 73:8]
    end
    uB <= reset | _GEN_2; // @[CPU.scala 69:{19,19}]
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  uR = _RAND_0[0:0];
  _RAND_1 = {1{`RANDOM}};
  uG = _RAND_1[0:0];
  _RAND_2 = {1{`RANDOM}};
  uB = _RAND_2[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
