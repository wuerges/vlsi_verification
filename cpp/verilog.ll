%{
#include "verilog.tab.hh"
%}
%%
and       return AND
or        return OR
buf       return BUF
xor       return XOR
xnor      return XNOR
not       return NOT
nor       return NOR
nand      return NAND
module    return MODULE
output    return OUTPUT
input     return INPUT
wire      return WIRE
endmodule return ENDMODULE
1\'b0     return VALUE1
