module CombALU(
  input         clock,
  input         reset,
  input  [63:0] io_dIn1,
  input  [63:0] io_dIn2,
  output [63:0] io_dOut,
  input  [3:0]  io_op,
  input  [2:0]  io_tIn1,
  input  [2:0]  io_tIn2,
  output [2:0]  io_tOut
);
  wire [2:0] _GEN_0 = io_tIn1 == 3'h5 & io_tIn2 == 3'h3 ? 3'h5 : 3'h0; // @[CombALU.scala 31:46 32:16 55:14]
  wire [2:0] _GEN_3 = io_tIn1 == 3'h5 & io_tIn2 == 3'h2 ? 3'h5 : _GEN_0; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_6 = io_tIn1 == 3'h5 & io_tIn2 == 3'h1 ? 3'h5 : _GEN_3; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_9 = io_tIn1 == 3'h5 & io_tIn2 == 3'h0 ? 3'h5 : _GEN_6; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_12 = io_tIn1 == 3'h5 & io_tIn2 == 3'h5 ? 3'h5 : _GEN_9; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_15 = io_tIn1 == 3'h5 & io_tIn2 == 3'h4 ? 3'h4 : _GEN_12; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_18 = io_tIn1 == 3'h4 & io_tIn2 == 3'h3 ? 3'h4 : _GEN_15; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_21 = io_tIn1 == 3'h4 & io_tIn2 == 3'h2 ? 3'h4 : _GEN_18; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_24 = io_tIn1 == 3'h4 & io_tIn2 == 3'h1 ? 3'h4 : _GEN_21; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_27 = io_tIn1 == 3'h4 & io_tIn2 == 3'h0 ? 3'h4 : _GEN_24; // @[CombALU.scala 31:46 32:16]
  wire [2:0] _GEN_30 = io_tIn1 == 3'h4 & io_tIn2 == 3'h5 ? 3'h4 : _GEN_27; // @[CombALU.scala 31:46 32:16]
  wire [2:0] tUpgrade = io_tIn1 == 3'h4 & io_tIn2 == 3'h4 ? 3'h4 : _GEN_30; // @[CombALU.scala 31:46 32:16]
  reg [63:0] result; // @[CombALU.scala 60:19]
  reg [2:0] resultType; // @[CombALU.scala 61:23]
  wire  _T_38 = tUpgrade == 3'h0; // @[CombALU.scala 70:20]
  wire [127:0] _result_T = io_dIn1 * io_dIn2; // @[CombALU.scala 71:28]
  wire [127:0] _GEN_39 = tUpgrade == 3'h0 ? _result_T : {{64'd0}, result}; // @[CombALU.scala 70:29 71:14 60:19]
  wire [2:0] _GEN_40 = tUpgrade == 3'h0 ? tUpgrade : resultType; // @[CombALU.scala 70:29 72:18 61:23]
  wire [63:0] _result_T_2 = io_dIn1 + io_dIn2; // @[CombALU.scala 77:28]
  wire [63:0] _GEN_41 = _T_38 ? _result_T_2 : result; // @[CombALU.scala 76:29 77:14 60:19]
  wire [63:0] _result_T_4 = io_dIn1 - io_dIn2; // @[CombALU.scala 83:28]
  wire [63:0] _GEN_43 = _T_38 ? _result_T_4 : result; // @[CombALU.scala 82:29 83:14 60:19]
  wire [63:0] _result_T_5 = io_dIn1 / io_dIn2; // @[CombALU.scala 89:28]
  wire [63:0] _GEN_45 = _T_38 ? _result_T_5 : result; // @[CombALU.scala 88:29 89:14 60:19]
  wire [63:0] _GEN_1 = io_dIn1 % io_dIn2; // @[CombALU.scala 95:28]
  wire [63:0] _result_T_6 = _GEN_1[63:0]; // @[CombALU.scala 95:28]
  wire [63:0] _GEN_47 = _T_38 ? _result_T_6 : result; // @[CombALU.scala 94:29 95:14 60:19]
  wire [63:0] _GEN_49 = io_op == 4'h9 ? {{63'd0}, io_dIn1 < io_dIn2} : result; // @[CombALU.scala 111:26 112:12 60:19]
  wire [2:0] _GEN_50 = io_op == 4'h9 ? 3'h3 : resultType; // @[CombALU.scala 111:26 113:16 61:23]
  wire [63:0] _GEN_51 = io_op == 4'h8 ? {{63'd0}, io_dIn1 >= io_dIn2} : _GEN_49; // @[CombALU.scala 107:26 108:12]
  wire [2:0] _GEN_52 = io_op == 4'h8 ? 3'h3 : _GEN_50; // @[CombALU.scala 107:26 109:16]
  wire [63:0] _GEN_53 = io_op == 4'h7 ? {{63'd0}, io_dIn1 != io_dIn2} : _GEN_51; // @[CombALU.scala 103:26 104:12]
  wire [2:0] _GEN_54 = io_op == 4'h7 ? 3'h3 : _GEN_52; // @[CombALU.scala 103:26 105:16]
  wire [63:0] _GEN_55 = io_op == 4'h6 ? {{63'd0}, io_dIn1 == io_dIn2} : _GEN_53; // @[CombALU.scala 100:12 99:26]
  wire [2:0] _GEN_56 = io_op == 4'h6 ? 3'h3 : _GEN_54; // @[CombALU.scala 101:16 99:26]
  wire [63:0] _GEN_57 = io_op == 4'h5 ? _GEN_47 : _GEN_55; // @[CombALU.scala 93:27]
  wire [2:0] _GEN_58 = io_op == 4'h5 ? _GEN_40 : _GEN_56; // @[CombALU.scala 93:27]
  wire [63:0] _GEN_59 = io_op == 4'h4 ? _GEN_45 : _GEN_57; // @[CombALU.scala 87:27]
  wire [2:0] _GEN_60 = io_op == 4'h4 ? _GEN_40 : _GEN_58; // @[CombALU.scala 87:27]
  wire [63:0] _GEN_61 = io_op == 4'h3 ? _GEN_43 : _GEN_59; // @[CombALU.scala 81:27]
  wire [63:0] _GEN_63 = io_op == 4'h2 ? _GEN_41 : _GEN_61; // @[CombALU.scala 75:27]
  wire [127:0] _GEN_65 = io_op == 4'h1 ? _GEN_39 : {{64'd0}, _GEN_63}; // @[CombALU.scala 69:27]
  wire [127:0] _GEN_68 = io_op == 4'h0 ? {{64'd0}, io_dIn2} : _GEN_65; // @[CombALU.scala 66:22 68:12]
  assign io_dOut = result; // @[CombALU.scala 63:8]
  assign io_tOut = resultType; // @[CombALU.scala 64:8]
  always @(posedge clock) begin
    result <= _GEN_68[63:0];
    if (io_op == 4'h0) begin // @[CombALU.scala 66:22]
      resultType <= io_tIn2; // @[CombALU.scala 67:16]
    end else if (io_op == 4'h1) begin // @[CombALU.scala 69:27]
      resultType <= _GEN_40;
    end else if (io_op == 4'h2) begin // @[CombALU.scala 75:27]
      resultType <= _GEN_40;
    end else if (io_op == 4'h3) begin // @[CombALU.scala 81:27]
      resultType <= _GEN_40;
    end else begin
      resultType <= _GEN_60;
    end
  end
endmodule
