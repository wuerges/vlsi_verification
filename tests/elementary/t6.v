
module top (a, b, c, y);
input a, b, c;
output y;

nand (y, a, b, c);

endmodule
