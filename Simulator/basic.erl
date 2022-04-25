-module(basic).
-export([main/1]).

-mode(compile).

% decode(<<2#0000:4, Vs:4>>) -> {load, decode_vs(Vs)};
% decode(<<2#0001:4, Vs:4>>) -> {mul,  decode_vs(Vs)};
% decode(<<2#0010:4, Vs:4>>) -> {add,  decode_vs(Vs)};
% decode(<<2#0011:4, Vs:4>>) -> {sub,  decode_vs(Vs)};
% decode(<<2#0100:4, Vs:4>>) -> {div_, decode_vs(Vs)};
% decode(<<2#0101:4, Vs:4>>) -> {rem_, decode_vs(Vs)};
% decode(<<2#0110:4, Vs:4>>) -> {not_, decode_vs(Vs)};
% decode(<<2#0111:4, Js:4>>) -> {jump, decode_js(Js)};
% decode(<<2#1000:4, Vs:4>>) -> {ge,   decode_vs(Vs)};
% decode(<<2#1001:4, Vs:4>>) -> {lt,   decode_vs(Vs)};
% decode(<<2#1010:4, Ts:4>>) -> {tcst, decode_ts(Ts)};
% decode(<<2#1011:4, Ts:4>>) -> {tcnv, decode_ts(Ts)};
% decode(<<2#1100:4, Vs:4>>) -> {put,  decode_vs(Vs)};
% decode(<<2#1101:4, Vs:4>>) -> {lsl,  decode_vs(Vs)};
% decode(<<2#1110:4, Vs:4>>) -> {and_, decode_vs(Vs)};
% decode(<<2#1111:4, Vs:4>>) -> {or_,  decode_vs(Vs)}.

run_jump(Js, Mem, Acc, Reg, Pc, Sp, Msp) -> {error, {run_jump, {Js, Mem, Acc, Reg, Pc, Sp, Msp}}}.

encode_typed(<<2#1000:4>>, V) -> <<0:32, V:32/float>>;
encode_typed(<<2#1001:4>>, V) -> <<V:64/float>>;
encode_typed(_, V) -> <<V:64>>.

run_tcnv(Ts, Mem, Acc, Reg, Pc, Sp, Msp) ->
  io:fwrite("Convert: ~p~n", [Ts]),
  {_, V} = decode_typed(Acc),
  Vr = do_bitmask(Ts, encode_typed(Ts, upgrade_to(V, Ts))),
  {Mem, <<Ts:4/bits, Vr:8/bytes>>, Reg, Pc, Sp, Msp}.

do_bitmask(<<_:2, 2#10:2>>, <<_,_,_,_,_,_,_,H>>) -> <<0,0,0,0,0,0,0,H>>;
do_bitmask(<<_:2, 2#11:2>>, <<_,_,_,_,_,_,G,H>>) -> <<0,0,0,0,0,0,G,H>>;
do_bitmask(<<_:2, 2#00:2>>, <<_,_,_,_,E,F,G,H>>) -> <<0,0,0,0,E,F,G,H>>;
do_bitmask(<<_:2, 2#01:2>>, <<A,B,C,D,E,F,G,H>>) -> <<A,B,C,D,E,F,G,H>>.

run_tcst(Ts, Mem, <<_:4, Acc/bytes>>, Reg, Pc, Sp, Msp) ->
  io:fwrite("Cast: ~p~n", [Ts]),
  {Mem, <<Ts:4/bits, (do_bitmask(Ts, Acc))/bytes>>, Reg, Pc, Sp, Msp}.

run_put(<<2#0110:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_i8}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#0111:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_i16}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#1000:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_i32}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#1001:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_i64}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#1010:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_f64}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#1011:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, stack_f64}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(<<2#1111:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  <<_:Pc/bytes, _:1, R:7, _/bits>> = Mem,
  io:fwrite("Put x~B~n", [R]),
  Nr = maps:put(R, Acc, Reg),
  {Mem, Acc, Nr, Pc + 1, Sp, Msp};
run_put(<<2#11:2, Vs:2>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  error({notimpl, {put, ref, Vs}}),
  {Mem, Acc, Reg, Pc, Sp, Msp};
run_put(_Vs, _Mem, _Acc, _Reg, _Pc, _Sp, _Msp) -> 
  error({put, literal}).

max_t(2#01, _) -> 2#01;
max_t(_, 2#01) -> 2#01;
max_t(2#00, _) -> 2#00;
max_t(_, 2#00) -> 2#00;
max_t(2#11, _) -> 2#11;
max_t(_, 2#11) -> 2#11;
max_t(_, _) -> 2#10.

% Unsigned
upgrade_type(<<2#00:2, T1:2>>, <<2#00:2, T2:2>>) -> <<2#00:2, (max_t(T1, T2)):2>>;
% Signed
upgrade_type(<<2#01:2, T1:2>>, <<0:1, _:1, T2:2>>) -> <<2#01:2, (max_t(T1, T2)):2>>;
upgrade_type(<<0:1, _:1, T1:2>>, <<2#01:2, T2:2>>) -> <<2#01:2, (max_t(T1, T2)):2>>;
% Float
upgrade_type(<<2#1001:4>>, _) -> <<2#1001:4>>;
upgrade_type(_, <<2#1001:4>>) -> <<2#1001:4>>;
upgrade_type(<<2#1000:4>>, _) -> <<2#1000:4>>;
upgrade_type(_, <<2#1000:4>>) -> <<2#1000:4>>.

upgrade_to(X, <<2#0010:4>>) ->
  <<_:56, R:8/unsigned>> = <<(trunc(X)):64>>,
  R;
upgrade_to(X, <<2#0011:4>>) ->
  <<_:48, R:16/unsigned>> = <<(trunc(X)):64>>,
  R;
upgrade_to(X, <<2#0000:4>>) ->
  <<_:32, R:32/unsigned>> = <<(trunc(X)):64>>,
  R;
upgrade_to(X, <<2#0001:4>>) ->
  <<R:64/unsigned>> = <<(trunc(X)):64>>,
  R;
upgrade_to(X, <<2#01:2, _:2>>) ->
  <<R:64/signed>> = <<(trunc(X)):64>>,
  R;
upgrade_to(X, <<2#100:3, _:1>>) -> float(X).

upgrade(V1, V2) ->
  {T1, X1} = decode_typed(V1),
  {T2, X2} = decode_typed(V2),
  Tr = upgrade_type(<<T1:4>>, <<T2:4>>),
  {Tr, upgrade_to(X1, Tr), upgrade_to(X2, Tr)}.

% Unsigned
make_acc(<<2#0010:4>>, Acc) -> <<2#0010:4, 0:56, Acc:8/unsigned>>;
make_acc(<<2#0011:4>>, Acc) -> <<2#0011:4, 0:48, Acc:16/unsigned>>;
make_acc(<<2#0000:4>>, Acc) -> <<2#0000:4, 0:32, Acc:32/unsigned>>;
make_acc(<<2#0001:4>>, Acc) -> <<2#0001:4, Acc:64/unsigned>>;
% Signed
make_acc(<<0:1, T:3>>, Acc) -> <<0:1, T:3, Acc:64/signed>>;
% Float
make_acc(<<2#1000:4>>, Acc) -> <<2#1000:4, 0:32, Acc:32/float>>;
make_acc(<<2#1001:4>>, Acc) -> <<2#1001:4, Acc:64/float>>;
% Exception
make_acc(T, _) -> error({not_a_type, T}).

% Unsigned
decode_typed(<<2#00:2, T:2, Acc:64/unsigned>>) -> {T, Acc};
% Signed
decode_typed(<<2#0110:4, _:56, Acc:8/signed>>) -> {2#0110, Acc};
decode_typed(<<2#0111:4, _:48, Acc:16/signed>>) -> {2#0111, Acc};
decode_typed(<<2#0100:4, _:32, Acc:32/signed>>) -> {2#0100, Acc};
decode_typed(<<2#0101:4, Acc:64/signed>>) -> {2#0101, Acc};
% Float
decode_typed(<<2#1000:4, _:32, Acc:32/float>>) -> {2#1000, Acc};
decode_typed(<<2#1001:4, Acc:64/float>>) -> {2#1001, Acc};
decode_typed(<<T:4, Acc>>) -> error({invalid_typed, {T, Acc}}).

run_div(A, B) when is_float(A) andalso is_float(B) -> A / B;
run_div(A, B) when is_integer(A) andalso is_integer(B) -> A div B.
run_rem(A, B) when is_float(A) andalso is_float(B) -> math:fmod(A, B);
run_rem(A, B) when is_integer(A) andalso is_integer(B) -> A rem B.

do_lsl_op(A, B) when is_integer(A) andalso B >= 0 ->
  io:fwrite("Lsl: ~p~n", [B]),
  A bsl B;
do_lsl_op(A, B) when is_integer(A) ->
  io:fwrite("Lsr: ~p~n", [B]),
  A bsr (-B);
do_lsl_op(A, B) when is_float(A) ->
  io:fwrite("Lsl: ~p~n", [B]),
  <<S:1, E:11, M:52>> = <<A:64/float>>,
  <<R:64/float>> = <<S:1, (E+B):11, M:52>>,
  R.

run_lsl(A, B) ->
  {Tr, Ta} = decode_typed(A),
  {_, Tb} = decode_typed(B),
  make_acc(<<Tr:4>>, do_lsl_op(Ta, trunc(Tb))).
  
run_op(2#0000, _, V) ->
  io:fwrite("Load: ~p~n", [element(2, decode_typed(V))]),
  V;
run_op(2#0001, Acc, V) ->
  {Tr, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Mul: ~p~n", [Tv]),
  make_acc(Tr, Ta * Tv);
run_op(2#0010, Acc, V) ->
  {Tr, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Add: ~p~n", [Tv]),
  make_acc(Tr, Ta + Tv);
run_op(2#0011, Acc, V) ->
  {Tr, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Sub: ~p~n", [Tv]),
  make_acc(Tr, Ta - Tv);
run_op(2#0100, Acc, V) ->
  {Tr, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Div: ~p~n", [Tv]),
  make_acc(Tr, run_div(Ta, Tv));
run_op(2#0101, Acc, V) ->
  {Tr, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Rem: ~p~n", [Tv]),
  make_acc(Tr, run_rem(Ta, Tv));
run_op(2#0110, Acc, V) ->
  <<_:4, A:64>> = Acc,
  <<_:4, B:64>> = V,
  io:fwrite("Xor: ~p~n", [B]),
  <<2#0001:4, (A bxor B):64/unsigned>>;
run_op(2#1000, Acc, V) ->
  {_, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Ge: ~p~n", [Tv]),
  <<2#0000:4, (if Ta >= Tv -> 1; true -> 0 end):64>>;
run_op(2#1001, Acc, V) ->
  {_, Ta, Tv} = upgrade(Acc, V),
  io:fwrite("Lt: ~p~n", [Tv]),
  <<2#0000:4, (if Ta < Tv -> 1; true -> 0 end):64>>;
run_op(2#1101, Acc, V) ->
  run_lsl(Acc, V);
  

run_op(Op, _, _) -> error({"Not an op", Op}).


run_instr(<<2#0111:4, Js:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> run_jump(<<Js:4>>, Mem, Acc, Reg, Pc, Sp, Msp);
run_instr(<<2#1010:4, Ts:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> run_tcst(<<Ts:4>>, Mem, Acc, Reg, Pc, Sp, Msp);
run_instr(<<2#1011:4, Ts:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> run_tcnv(<<Ts:4>>, Mem, Acc, Reg, Pc, Sp, Msp);
run_instr(<<2#1100:4, Vs:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> run_put(<<Vs:4>>, Mem, Acc, Reg, Pc, Sp, Msp);

run_instr(<<Op:4, 2#0010:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  <<_:Pc/bytes, X:1/bytes, _/bits>> = Mem,
  <<T:1/signed, _:7>> = X,
  {Mem, run_op(Op, Acc, <<2#0010:4, T:56, X/bytes>>), Reg, Pc + 1, Sp, Msp};
run_instr(<<Op:4, 2#0011:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> 
  <<_:Pc/bytes, X:2/bytes, _/bits>> = Mem,
  <<T:1/signed, _:15>> = X,
  {Mem, run_op(Op, Acc, <<2#0011:4, T:48, X/bytes>>), Reg, Pc + 2, Sp, Msp};
run_instr(<<Op:4, 2#0000:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> 
  <<_:Pc/bytes, X:4/bytes, _/bits>> = Mem,
  <<T:1/signed, _:31>> = X,
  {Mem, run_op(Op, Acc, <<2#0000:4, T:32, X/bytes>>), Reg, Pc + 4, Sp, Msp};
run_instr(<<Op:4, 2#0001:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> 
  <<_:Pc/bytes, X:8/bytes, _/bits>> = Mem,
  {Mem, run_op(Op, Acc, <<2#0001:4, X/bytes>>), Reg, Pc + 8, Sp, Msp};
run_instr(<<Op:4, 2#0100:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> 
  <<_:Pc/bytes, X:4/bytes, _/bits>> = Mem,
  {Mem, run_op(Op, Acc, <<2#1000:4, X/bytes>>), Reg, Pc + 4, Sp, Msp};
run_instr(<<Op:4, 2#0101:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  <<_:Pc/bytes, X:8/bytes, _/bits>> = Mem,
  {Mem, run_op(Op, Acc, <<2#1001:4, X/bytes>>), Reg, Pc + 8, Sp, Msp};

run_instr(<<Op:4, 2#0110:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_1i;
run_instr(<<Op:4, 2#0111:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_1i;
run_instr(<<Op:4, 2#1000:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_4i;
run_instr(<<Op:4, 2#1001:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_8i;
run_instr(<<Op:4, 2#1010:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_4f;
run_instr(<<Op:4, 2#1011:4>>, Mem, Acc, Reg, Pc, Sp, Msp) -> do_stack_8f;

run_instr(<<Op:4, 2#1110:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  % Reg:
  %   0 => ref: 8 bit sint
  %   1 => ref: 16 bit sint
  {error, {notimpl, {ref, s8s16}}};
run_instr(<<Op:4, 2#1100:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  % Reg:
  %   0 => ref: 32 bit sint
  %   1 => ref: 64 bit sint
  {error, {notimpl, {ref, s32s64}}};
run_instr(<<Op:4, 2#1101:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  % Reg:
  %   0 => ref: 32 bit float
  %   1 => ref: 64 bit float
  {error, {notimpl, {ref, f32f64}}};
run_instr(<<Op:4, 2#1111:4>>, Mem, Acc, Reg, Pc, Sp, Msp) ->
  <<_:Pc/bytes, _:1, R:7, _/bits>> = Mem,
  {Mem, run_op(Op, Acc, maps:get(R, Reg)), Reg, Pc + 1, Sp, Msp};

run_instr(Op, _Mem, _Acc, _Reg, _Pc, _Sp, _Msp) ->
  {error, {unexpected, Op}}.

reg_to_str(Reg) ->
  lists:flatten([io_lib:format("~n  ~B: ~p", [X, decode_typed(Y)]) || {X,Y} <- lists:sort(maps:to_list(Reg))]).

interpret(Mem, Acc, Reg, Pc, Sp, Msp) ->
  io:fwrite("~n"),
  <<_:Pc/bytes, I:1/bytes, _/bits>> = Mem,
  {Nm, Na, Nr, Np, Ns, Nu} = run_instr(I, Mem, Acc, Reg, Pc + 1, Sp, Msp),
  if
    Np < byte_size(Nm) ->
      io:fwrite("Acc: ~p~n", [decode_typed(Na)]),
      interpret(Nm, Na, Nr, Np, Ns, Nu);
    true ->
      io:fwrite("~nMem: ~w~nAcc: ~p~nReg: ~s~n", [Nm, decode_typed(Na), reg_to_str(Nr)])
  end.

main([File]) ->
  {ok, F} = file:read_file(File),
  io:fwrite("File: ~w~n", [F]),
  interpret(F, <<0:68>>, maps:from_keys(lists:seq(0, 127), <<0:68>>), 0, byte_size(F), 0).