module TxPipeline(
  input        clock,
  input        reset,
  input        i_oe,
  output [1:0] i_iosg,
  output [7:0] i_iosp
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] stateGrey; // @[TxPipeline.scala 11:22]
  reg [7:0] syncPulse; // @[TxPipeline.scala 12:22]
  reg [1:0] state; // @[FSM.scala 10:18]
  wire [1:0] _GEN_0 = i_oe ? 2'h1 : state; // @[TxPipeline.scala 17:19 18:15 FSM.scala 10:18]
  wire [1:0] _GEN_3 = state == 2'h0 ? _GEN_0 : state; // @[FSM.scala 10:18 TxPipeline.scala 16:29]
  assign i_iosg = stateGrey; // @[TxPipeline.scala 14:10]
  assign i_iosp = syncPulse; // @[TxPipeline.scala 13:10]
  always @(posedge clock) begin
    if (state == 2'h1) begin // @[TxPipeline.scala 26:34]
      if (syncPulse[0]) begin // @[TxPipeline.scala 28:27]
        stateGrey <= 2'h3; // @[TxPipeline.scala 30:19]
      end else begin
        stateGrey <= 2'h1; // @[TxPipeline.scala 32:19]
      end
    end else if (state == 2'h0) begin // @[TxPipeline.scala 16:29]
      stateGrey <= {{1'd0}, i_oe};
    end
    if (state == 2'h1) begin // @[TxPipeline.scala 26:34]
      syncPulse <= {{1'd0}, syncPulse[7:1]}; // @[TxPipeline.scala 27:17]
    end else if (state == 2'h0) begin // @[TxPipeline.scala 16:29]
      if (i_oe) begin // @[TxPipeline.scala 17:19]
        syncPulse <= 8'h80; // @[TxPipeline.scala 19:19]
      end
    end
    if (state == 2'h1) begin // @[TxPipeline.scala 26:34]
      if (syncPulse[0]) begin // @[TxPipeline.scala 28:27]
        state <= 2'h2; // @[TxPipeline.scala 29:15]
      end else begin
        state <= _GEN_3;
      end
    end else begin
      state <= _GEN_3;
    end
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
  stateGrey = _RAND_0[1:0];
  _RAND_1 = {1{`RANDOM}};
  syncPulse = _RAND_1[7:0];
  _RAND_2 = {1{`RANDOM}};
  state = _RAND_2[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
