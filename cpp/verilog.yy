%{
#include <iostream>
int yylex(void);
void yyerror(const char *);
%}
%token AND
%token OR
%token BUF
%token XOR
%token XNOR
%token NOT
%token NOR
%token NAND
%token MODULE
%token OUTPUT
%token INPUT
%token WIRE
%token ENDMODULE
%token VALUE1
%start statement
%%
statement : MODULE ;
%%
using namespace std;
int main(int nargs, char** argv) {
}
