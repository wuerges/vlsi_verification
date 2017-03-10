
module top (a, b, c, y);
input a, b, c;
output y;

wire x;

and (x, a, b, c);
not (y, x);

endmodule
