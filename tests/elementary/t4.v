

module top (a, b, c, y);
input a, b, c;
output y;

wire k, l, m;

or (k, a, b, c);
and (l, a, b, c);
xor (m, a, b, c);

and (y, k, l, m);

endmodule
