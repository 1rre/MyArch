module CPU_TOP (
    input   clk48,
    output  rst_n,
    input   usr_btn,
    output  rgb_led0_r,
    output  rgb_led0_g,
    output  rgb_led0_b
  );

  CPU cpu (
    .clock(clk48),
    .rst_n(rst_n),
    .usr_btn(usr_btn),
    .rgb_led0_r(rgb_led0_r),
    .rgb_led0_g(rgb_led0_g),
    .rgb_led0_b(rgb_led0_b)
  );

endmodule