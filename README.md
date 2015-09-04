## Synopsis

This project is:

1. A solution to the [Problem B](http://cad-contest.el.cycu.edu.tw/problem_B/default.htm) of the ICCAD 2015 Contest.

points on large designs in an effort to reduce their equivalence checking and function change problems. 

2. A verilog netlist parser written in haskell with [Parsec](https://wiki.haskell.org/Parsec). It turns a verilog netlist into a graph, with all ports converted to And an Not (nand synthesis), than can be easily manipulated using [fgl](http://hackage.haskell.org/package/fgl).

3. A ***ORBDD*** (Ordered Reduced Binary Decision Diagrams) creation and manipulation framework. ORBDD represent combinational circuits in canonical form, that is, the ORBDDs of equivalent predicates are equal. For this reason, ORBDDs are specialy useful to check equivalence of combinational circuits.

## Code Examples

Since the project is in its beginnings, there is not much in this section.

But just for a taste, the following code parses a verilog file into a graph and
prints it into a format that can be used with graphviz:
```Haskell
main :: IO ()
main = do 
          [f] <- getArgs
          p <- parseVerilog f
          case p of
            Right r -> do putStrLn $ showGraph g
                          where vi = verilogToInt r (attIndexV emptyIndex r)
                                g = makeGraphV vi
            Left l ->  error $ show l
```

## Motivation

This is a quote from the Problem B description:

> Efficient equivalence checking and functional correction on large-scale designs are crucial technologies for handling today’s demanding design cycles. Analyzing an entire design is not always practical for large-scale designs because of their size and complexity. It is well known that partitioning the design, based on two designs’ correspondence, can significantly reduce the complexity of the analysis. 

> Identifying corresponding cuts is an effective method of simplifying the equivalence checking problem caused by the Boolean complexity of large-scale designs. The corresponding cuts represent points of functional correspondence between the two designs, allowing you to partition the designs into smaller sub-problems. However, finding the equivalent correspondence can be difficult when the design has gone through ODC/SDC optimization. An algorithm that can effectively determine functional correspondence, can help improve the success of large-scale equivalence checking. 

In other words, testing circuits is not easy and we need more tools to do it.

## Installation

TODO

## API Reference

Since API is not yet defined, it is not wise yet to publish it.

## Tests

No automated tests are available (yet).

## Contributors

For now, only me. But I welcome contributors.

## License

The current code is available under BSD3 licese. 

>
>Copyright (c) 2015, Emilio Wuerges
>
>All rights reserved.
>
>Redistribution and use in source and binary forms, with or without
>modification, are permitted provided that the following conditions are met:
>
>    * Redistributions of source code must retain the above copyright
>      notice, this list of conditions and the following disclaimer.
>
>    * Redistributions in binary form must reproduce the above
>      copyright notice, this list of conditions and the following
>      disclaimer in the documentation and/or other materials provided
>      with the distribution.
>
>    * Neither the name of Emilio Wuerges nor the names of other
>      contributors may be used to endorse or promote products derived
>      from this software without specific prior written permission.
>
>THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
>"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
>LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
>A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
>OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
>SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
>LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
>DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
>THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
>(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
>OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
>
