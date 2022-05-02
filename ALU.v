module Unsigned(
  input  [63:0] io_in1,
  input  [63:0] io_in2,
  input  [3:0]  io_op,
  output [63:0] io_out_d
);
  wire  _io_out_d_T = io_op == 4'h0; // @[Unsigned.scala 27:9]
  wire  _io_out_d_T_1 = io_op == 4'h1; // @[Unsigned.scala 28:9]
  wire [127:0] _io_out_d_T_2 = io_in1 * io_in2; // @[Unsigned.scala 28:28]
  wire  _io_out_d_T_4 = io_op == 4'h2; // @[Unsigned.scala 29:9]
  wire [63:0] _io_out_d_T_6 = io_in1 + io_in2; // @[Unsigned.scala 29:28]
  wire  _io_out_d_T_7 = io_op == 4'h3; // @[Unsigned.scala 30:9]
  wire [63:0] _io_out_d_T_9 = io_in1 - io_in2; // @[Unsigned.scala 30:28]
  wire  _io_out_d_T_10 = io_op == 4'h4; // @[Unsigned.scala 31:9]
  wire [63:0] _io_out_d_T_11 = io_in1 / io_in2; // @[Unsigned.scala 31:28]
  wire  _io_out_d_T_12 = io_op == 4'h5; // @[Unsigned.scala 32:9]
  wire [63:0] _GEN_0 = io_in1 % io_in2; // @[Unsigned.scala 32:28]
  wire [63:0] _io_out_d_T_13 = _GEN_0[63:0]; // @[Unsigned.scala 32:28]
  wire  _io_out_d_T_14 = io_op == 4'he; // @[Unsigned.scala 33:9]
  wire [63:0] _io_out_d_T_15 = io_in1 & io_in2; // @[Unsigned.scala 33:28]
  wire  _io_out_d_T_16 = io_op == 4'hf; // @[Unsigned.scala 34:9]
  wire [63:0] _io_out_d_T_17 = io_in1 | io_in2; // @[Unsigned.scala 34:28]
  wire  _io_out_d_T_18 = io_op == 4'h6; // @[Unsigned.scala 35:9]
  wire  _io_out_d_T_19 = io_in1 == io_in2; // @[Unsigned.scala 35:41]
  wire [63:0] _io_out_d_T_20 = {63'h0,_io_out_d_T_19}; // @[Cat.scala 31:58]
  wire  _io_out_d_T_21 = io_op == 4'h7; // @[Unsigned.scala 36:9]
  wire  _io_out_d_T_22 = io_in1 != io_in2; // @[Unsigned.scala 36:41]
  wire [63:0] _io_out_d_T_23 = {63'h0,_io_out_d_T_22}; // @[Cat.scala 31:58]
  wire  _io_out_d_T_24 = io_op == 4'h8; // @[Unsigned.scala 37:9]
  wire  _io_out_d_T_25 = io_in1 >= io_in2; // @[Unsigned.scala 37:42]
  wire [63:0] _io_out_d_T_26 = {63'h0,_io_out_d_T_25}; // @[Cat.scala 31:58]
  wire  _io_out_d_T_27 = io_op == 4'h9; // @[Unsigned.scala 38:9]
  wire  _io_out_d_T_28 = io_in1 < io_in2; // @[Unsigned.scala 38:42]
  wire [63:0] _io_out_d_T_29 = {63'h0,_io_out_d_T_28}; // @[Cat.scala 31:58]
  wire  _io_out_d_T_30 = io_op == 4'hd; // @[Unsigned.scala 39:9]
  wire [63:0] _io_out_d_T_33 = $signed(io_in2) < 64'sh7f ? $signed(io_in2) : $signed(64'sh7f); // @[Unsigned.scala 22:43]
  wire [63:0] _io_out_d_T_37 = $signed(_io_out_d_T_33) < -64'sh80 ? $signed(-64'sh80) : $signed(_io_out_d_T_33); // @[Unsigned.scala 22:64]
  wire [7:0] _io_out_d_WIRE_1 = _io_out_d_T_37[7:0]; // @[Unsigned.scala 22:{64,64}]
  wire [7:0] _io_out_d_T_46 = _io_out_d_T_37[7:0]; // @[Unsigned.scala 23:40]
  wire [318:0] _GEN_1 = {{255'd0}, io_in1}; // @[Unsigned.scala 23:29]
  wire [318:0] _io_out_d_T_47 = _GEN_1 << _io_out_d_T_46; // @[Unsigned.scala 23:29]
  wire [7:0] _io_out_d_T_58 = 8'sh0 - $signed(_io_out_d_WIRE_1); // @[Unsigned.scala 23:66]
  wire [63:0] _io_out_d_T_59 = io_in1 >> _io_out_d_T_58; // @[Unsigned.scala 23:52]
  wire [318:0] _io_out_d_T_60 = $signed(_io_out_d_WIRE_1) >= 8'sh0 ? _io_out_d_T_47 : {{255'd0}, _io_out_d_T_59}; // @[Unsigned.scala 23:8]
  wire [63:0] _io_out_d_T_62 = _io_out_d_T ? io_in2 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_63 = _io_out_d_T_1 ? _io_out_d_T_2[63:0] : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_64 = _io_out_d_T_4 ? _io_out_d_T_6 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_65 = _io_out_d_T_7 ? _io_out_d_T_9 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_66 = _io_out_d_T_10 ? _io_out_d_T_11 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_67 = _io_out_d_T_12 ? _io_out_d_T_13 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_68 = _io_out_d_T_14 ? _io_out_d_T_15 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_69 = _io_out_d_T_16 ? _io_out_d_T_17 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_70 = _io_out_d_T_18 ? _io_out_d_T_20 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_71 = _io_out_d_T_21 ? _io_out_d_T_23 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_72 = _io_out_d_T_24 ? _io_out_d_T_26 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_73 = _io_out_d_T_27 ? _io_out_d_T_29 : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_74 = _io_out_d_T_30 ? _io_out_d_T_60[63:0] : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_75 = _io_out_d_T_62 | _io_out_d_T_63; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_76 = _io_out_d_T_75 | _io_out_d_T_64; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_77 = _io_out_d_T_76 | _io_out_d_T_65; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_78 = _io_out_d_T_77 | _io_out_d_T_66; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_79 = _io_out_d_T_78 | _io_out_d_T_67; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_80 = _io_out_d_T_79 | _io_out_d_T_68; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_81 = _io_out_d_T_80 | _io_out_d_T_69; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_82 = _io_out_d_T_81 | _io_out_d_T_70; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_83 = _io_out_d_T_82 | _io_out_d_T_71; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_84 = _io_out_d_T_83 | _io_out_d_T_72; // @[Mux.scala 27:73]
  wire [63:0] _io_out_d_T_85 = _io_out_d_T_84 | _io_out_d_T_73; // @[Mux.scala 27:73]
  assign io_out_d = _io_out_d_T_85 | _io_out_d_T_74; // @[Mux.scala 27:73]
endmodule
module Signed(
  input  [63:0] io_in1,
  input  [63:0] io_in2,
  input  [3:0]  io_op,
  output [63:0] io_out_d,
  output [1:0]  io_out_t
);
  wire  _outS_T = io_op == 4'h0; // @[Signed.scala 28:9]
  wire [63:0] _outS_T_76 = io_in2; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_12 = {{1{_outS_T_76[63]}},_outS_T_76}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_101 = _outS_T ? $signed(_outS_WIRE_12) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire  _outS_T_1 = io_op == 4'h1; // @[Signed.scala 29:9]
  wire [127:0] _outS_T_2 = $signed(io_in1) * $signed(io_in2); // @[Signed.scala 29:28]
  wire [63:0] _outS_T_78 = _outS_T_2[63:0]; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_13 = {{1{_outS_T_78[63]}},_outS_T_78}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_102 = _outS_T_1 ? $signed(_outS_WIRE_13) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_115 = $signed(_outS_T_101) | $signed(_outS_T_102); // @[Mux.scala 27:73]
  wire  _outS_T_5 = io_op == 4'h2; // @[Signed.scala 30:9]
  wire [63:0] _outS_T_80 = $signed(io_in1) + $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_14 = {{1{_outS_T_80[63]}},_outS_T_80}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_103 = _outS_T_5 ? $signed(_outS_WIRE_14) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_117 = $signed(_outS_T_115) | $signed(_outS_T_103); // @[Mux.scala 27:73]
  wire  _outS_T_9 = io_op == 4'h3; // @[Signed.scala 31:9]
  wire [63:0] _outS_T_82 = $signed(io_in1) - $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_15 = {{1{_outS_T_82[63]}},_outS_T_82}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_104 = _outS_T_9 ? $signed(_outS_WIRE_15) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_119 = $signed(_outS_T_117) | $signed(_outS_T_104); // @[Mux.scala 27:73]
  wire  _outS_T_13 = io_op == 4'h4; // @[Signed.scala 32:9]
  wire [64:0] _outS_T_84 = $signed(io_in1) / $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_105 = _outS_T_13 ? $signed(_outS_T_84) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_121 = $signed(_outS_T_119) | $signed(_outS_T_105); // @[Mux.scala 27:73]
  wire  _outS_T_15 = io_op == 4'h5; // @[Signed.scala 33:9]
  wire [63:0] _outS_T_86 = $signed(io_in1) % $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_17 = {{1{_outS_T_86[63]}},_outS_T_86}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_106 = _outS_T_15 ? $signed(_outS_WIRE_17) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_123 = $signed(_outS_T_121) | $signed(_outS_T_106); // @[Mux.scala 27:73]
  wire  _outS_T_17 = io_op == 4'he; // @[Signed.scala 34:9]
  wire [63:0] _outS_T_88 = $signed(io_in1) & $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_18 = {{1{_outS_T_88[63]}},_outS_T_88}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_107 = _outS_T_17 ? $signed(_outS_WIRE_18) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_125 = $signed(_outS_T_123) | $signed(_outS_T_107); // @[Mux.scala 27:73]
  wire  _outS_T_20 = io_op == 4'hf; // @[Signed.scala 35:9]
  wire [63:0] _outS_T_90 = $signed(io_in1) | $signed(io_in2); // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_19 = {{1{_outS_T_90[63]}},_outS_T_90}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_108 = _outS_T_20 ? $signed(_outS_WIRE_19) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_127 = $signed(_outS_T_125) | $signed(_outS_T_108); // @[Mux.scala 27:73]
  wire  _outS_T_23 = io_op == 4'h6; // @[Signed.scala 36:9]
  wire  _outS_T_24 = $signed(io_in1) == $signed(io_in2); // @[Signed.scala 36:41]
  wire [63:0] _outS_T_92 = {63'h0,_outS_T_24}; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_20 = {{1{_outS_T_92[63]}},_outS_T_92}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_109 = _outS_T_23 ? $signed(_outS_WIRE_20) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_129 = $signed(_outS_T_127) | $signed(_outS_T_109); // @[Mux.scala 27:73]
  wire  _outS_T_27 = io_op == 4'h7; // @[Signed.scala 37:9]
  wire  _outS_T_28 = $signed(io_in1) != $signed(io_in2); // @[Signed.scala 37:41]
  wire [63:0] _outS_T_94 = {63'h0,_outS_T_28}; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_21 = {{1{_outS_T_94[63]}},_outS_T_94}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_110 = _outS_T_27 ? $signed(_outS_WIRE_21) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_131 = $signed(_outS_T_129) | $signed(_outS_T_110); // @[Mux.scala 27:73]
  wire  _outS_T_31 = io_op == 4'h8; // @[Signed.scala 38:9]
  wire  _outS_T_32 = $signed(io_in1) >= $signed(io_in2); // @[Signed.scala 38:42]
  wire [63:0] _outS_T_96 = {63'h0,_outS_T_32}; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_22 = {{1{_outS_T_96[63]}},_outS_T_96}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_111 = _outS_T_31 ? $signed(_outS_WIRE_22) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_133 = $signed(_outS_T_131) | $signed(_outS_T_111); // @[Mux.scala 27:73]
  wire  _outS_T_35 = io_op == 4'h9; // @[Signed.scala 39:9]
  wire  _outS_T_36 = $signed(io_in1) < $signed(io_in2); // @[Signed.scala 39:42]
  wire [63:0] _outS_T_98 = {63'h0,_outS_T_36}; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_23 = {{1{_outS_T_98[63]}},_outS_T_98}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_112 = _outS_T_35 ? $signed(_outS_WIRE_23) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_135 = $signed(_outS_T_133) | $signed(_outS_T_112); // @[Mux.scala 27:73]
  wire  _outS_T_39 = io_op == 4'hd; // @[Signed.scala 40:9]
  wire [63:0] _outS_T_43 = $signed(io_in2) < 64'sh7f ? $signed(io_in2) : $signed(64'sh7f); // @[Signed.scala 23:43]
  wire [63:0] _outS_T_47 = $signed(_outS_T_43) < -64'sh80 ? $signed(-64'sh80) : $signed(_outS_T_43); // @[Signed.scala 23:64]
  wire [7:0] _outS_WIRE_6 = _outS_T_47[7:0]; // @[Signed.scala 23:{64,64}]
  wire [7:0] _outS_T_57 = _outS_T_47[7:0]; // @[Signed.scala 24:40]
  wire [318:0] _GEN_1 = {{255{io_in1[63]}},io_in1}; // @[Signed.scala 24:29]
  wire [318:0] _outS_T_58 = $signed(_GEN_1) << _outS_T_57; // @[Signed.scala 24:29]
  wire [7:0] _outS_T_70 = 8'sh0 - $signed(_outS_WIRE_6); // @[Signed.scala 24:66]
  wire [63:0] _outS_T_71 = $signed(io_in1) >>> _outS_T_70; // @[Signed.scala 24:52]
  wire [318:0] _outS_T_72 = $signed(_outS_WIRE_6) >= 8'sh0 ? $signed(_outS_T_58) : $signed({{255{_outS_T_71[63]}},
    _outS_T_71}); // @[Signed.scala 24:8]
  wire [63:0] _outS_T_100 = _outS_T_72[63:0]; // @[Mux.scala 27:73]
  wire [64:0] _outS_WIRE_24 = {{1{_outS_T_100[63]}},_outS_T_100}; // @[Mux.scala 27:{73,73}]
  wire [64:0] _outS_T_113 = _outS_T_39 ? $signed(_outS_WIRE_24) : $signed(65'sh0); // @[Mux.scala 27:73]
  wire [64:0] _outS_T_139 = $signed(_outS_T_135) | $signed(_outS_T_113); // @[Mux.scala 27:73]
  wire [1:0] _io_out_t_T_4 = _outS_T_35 ? 2'h1 : 2'h0; // @[Mux.scala 101:16]
  wire [1:0] _io_out_t_T_5 = _outS_T_31 ? 2'h1 : _io_out_t_T_4; // @[Mux.scala 101:16]
  wire [1:0] _io_out_t_T_6 = _outS_T_27 ? 2'h1 : _io_out_t_T_5; // @[Mux.scala 101:16]
  assign io_out_d = _outS_T_139[63:0]; // @[Signed.scala 20:25]
  assign io_out_t = _outS_T_23 ? 2'h1 : _io_out_t_T_6; // @[Mux.scala 101:16]
endmodule
module ALU(
  input         clock,
  input         reset,
  input  [63:0] io_in1_d,
  input  [1:0]  io_in1_t,
  input  [63:0] io_in2_d,
  input  [1:0]  io_in2_t,
  output [63:0] io_out_d,
  output [1:0]  io_out_t,
  input  [3:0]  io_op
);
`ifdef RANDOMIZE_REG_INIT
  reg [63:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  wire [63:0] unsignedALU_io_in1; // @[ALU.scala 64:27]
  wire [63:0] unsignedALU_io_in2; // @[ALU.scala 64:27]
  wire [3:0] unsignedALU_io_op; // @[ALU.scala 64:27]
  wire [63:0] unsignedALU_io_out_d; // @[ALU.scala 64:27]
  wire [63:0] signedALU_io_in1; // @[ALU.scala 69:25]
  wire [63:0] signedALU_io_in2; // @[ALU.scala 69:25]
  wire [3:0] signedALU_io_op; // @[ALU.scala 69:25]
  wire [63:0] signedALU_io_out_d; // @[ALU.scala 69:25]
  wire [1:0] signedALU_io_out_t; // @[ALU.scala 69:25]
  wire [1:0] _GEN_0 = io_in1_t == 2'h1 & io_in1_t == 2'h0 ? 2'h1 : io_in1_t; // @[ALU.scala 34:48 35:16 56:14]
  wire [1:0] _GEN_3 = io_in1_t == 2'h0 & io_in1_t == 2'h1 ? 2'h1 : _GEN_0; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_6 = io_in1_t == 2'h2 & io_in1_t == 2'h1 ? 2'h2 : _GEN_3; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_9 = io_in1_t == 2'h2 & io_in1_t == 2'h0 ? 2'h2 : _GEN_6; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_12 = io_in1_t == 2'h2 & io_in1_t == 2'h2 ? 2'h2 : _GEN_9; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_15 = io_in1_t == 2'h2 & io_in1_t == 2'h3 ? 2'h3 : _GEN_12; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_18 = io_in1_t == 2'h3 & io_in1_t == 2'h1 ? 2'h3 : _GEN_15; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_21 = io_in1_t == 2'h3 & io_in1_t == 2'h0 ? 2'h3 : _GEN_18; // @[ALU.scala 34:48 35:16]
  wire [1:0] _GEN_24 = io_in1_t == 2'h3 & io_in1_t == 2'h2 ? 2'h3 : _GEN_21; // @[ALU.scala 34:48 35:16]
  wire [1:0] tUpgrade = io_in1_t == 2'h3 & io_in1_t == 2'h3 ? 2'h3 : _GEN_24; // @[ALU.scala 34:48 35:16]
  reg [63:0] result_d; // @[ALU.scala 61:19]
  reg [1:0] result_t; // @[ALU.scala 61:19]
  wire  _result_T = tUpgrade == 2'h1; // @[ALU.scala 75:15]
  wire  _result_T_1 = tUpgrade == 2'h0; // @[ALU.scala 76:15]
  wire [1:0] _result_T_2 = _result_T ? 2'h1 : 2'h0; // @[Mux.scala 27:73]
  wire [1:0] _result_T_3 = _result_T_1 ? signedALU_io_out_t : 2'h0; // @[Mux.scala 27:73]
  wire [63:0] _result_T_5 = _result_T ? unsignedALU_io_out_d : 64'h0; // @[Mux.scala 27:73]
  wire [63:0] _result_T_6 = _result_T_1 ? signedALU_io_out_d : 64'h0; // @[Mux.scala 27:73]
  Unsigned unsignedALU ( // @[ALU.scala 64:27]
    .io_in1(unsignedALU_io_in1),
    .io_in2(unsignedALU_io_in2),
    .io_op(unsignedALU_io_op),
    .io_out_d(unsignedALU_io_out_d)
  );
  Signed signedALU ( // @[ALU.scala 69:25]
    .io_in1(signedALU_io_in1),
    .io_in2(signedALU_io_in2),
    .io_op(signedALU_io_op),
    .io_out_d(signedALU_io_out_d),
    .io_out_t(signedALU_io_out_t)
  );
  assign io_out_d = result_d; // @[ALU.scala 62:7]
  assign io_out_t = result_t; // @[ALU.scala 62:7]
  assign unsignedALU_io_in1 = io_in1_d; // @[ALU.scala 51:24 53:16]
  assign unsignedALU_io_in2 = io_in2_d; // @[ALU.scala 51:24 54:16]
  assign unsignedALU_io_op = io_op; // @[ALU.scala 67:21]
  assign signedALU_io_in1 = io_in1_d; // @[ALU.scala 70:42]
  assign signedALU_io_in2 = io_in2_d; // @[ALU.scala 71:42]
  assign signedALU_io_op = io_op; // @[ALU.scala 72:19]
  always @(posedge clock) begin
    result_d <= _result_T_5 | _result_T_6; // @[Mux.scala 27:73]
    result_t <= _result_T_2 | _result_T_3; // @[Mux.scala 27:73]
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
  _RAND_0 = {2{`RANDOM}};
  result_d = _RAND_0[63:0];
  _RAND_1 = {1{`RANDOM}};
  result_t = _RAND_1[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
