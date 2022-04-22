circuit TxPipeline :
  module TxPipeline :
    input clock : Clock
    input reset : UInt<1>
    output i : { flip oe : UInt<1>, iosg : UInt<2>, iosp : UInt<8>}

    reg stateGrey : UInt<2>, clock with :
      reset => (UInt<1>("h0"), stateGrey) @[TxPipeline.scala 11:22]
    reg syncPulse : UInt<8>, clock with :
      reset => (UInt<1>("h0"), syncPulse) @[TxPipeline.scala 12:22]
    i.iosp <= syncPulse @[TxPipeline.scala 13:10]
    i.iosg <= stateGrey @[TxPipeline.scala 14:10]
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[FSM.scala 10:18]
    node _T = eq(state, UInt<2>("h0")) @[TxPipeline.scala 16:17]
    when _T : @[TxPipeline.scala 16:29]
      when i.oe : @[TxPipeline.scala 17:19]
        state <= UInt<2>("h1") @[TxPipeline.scala 18:15]
        syncPulse <= UInt<8>("h80") @[TxPipeline.scala 19:19]
        stateGrey <= UInt<1>("h1") @[TxPipeline.scala 20:19]
      else :
        stateGrey <= UInt<1>("h0") @[TxPipeline.scala 22:19]
    node _T_1 = eq(state, UInt<2>("h1")) @[TxPipeline.scala 26:17]
    when _T_1 : @[TxPipeline.scala 26:34]
      node _syncPulse_T = shr(syncPulse, 1) @[TxPipeline.scala 27:30]
      syncPulse <= _syncPulse_T @[TxPipeline.scala 27:17]
      node _T_2 = bits(syncPulse, 0, 0) @[TxPipeline.scala 28:22]
      when _T_2 : @[TxPipeline.scala 28:27]
        state <= UInt<2>("h2") @[TxPipeline.scala 29:15]
        stateGrey <= UInt<2>("h3") @[TxPipeline.scala 30:19]
      else :
        stateGrey <= UInt<1>("h1") @[TxPipeline.scala 32:19]

