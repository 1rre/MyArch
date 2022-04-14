
module CPU_TOP (
    input   clk48,
    output  rst_n,
    input   usr_btn,
    output  rgb_led0_r,
    output  rgb_led0_g,
    output  rgb_led0_b
  );

  reg ur, ug, ub;

  initial begin
    ur <= 1;
    ug <= 0;
    ub <= 0;  
  end

  always @(posedge clk48) begin
    if (usr_btn) begin
      ur <= 1;
      ug <= 0;
      ub <= 1;
    end else begin
      ur <= 0;
      ug <= 1;
      ub <= 1;
    end
  end

  assign rgb_led0_r = ur;
  assign rgb_led0_g = ug;
  assign rgb_led0_b = ub;

  assign rst_n = 1;

endmodule