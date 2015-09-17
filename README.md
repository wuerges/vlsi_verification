## Synopsis

This project is:

1. A solution to the [Problem B](http://cad-contest.el.cycu.edu.tw/problem_B/default.htm) of the ICCAD 2015 Contest.


2. A verilog netlist parser written in haskell with [Parsec](https://wiki.haskell.org/Parsec). It turns a verilog netlist into a graph, with all ports converted to And an Not (nand synthesis), than can be easily manipulated using [fgl](http://hackage.haskell.org/package/fgl).

3. A ***ORBDD*** (Ordered Reduced Binary Decision Diagrams) creation and manipulation framework. ORBDD represent combinational circuits in canonical form, that is, the ORBDDs of equivalent predicates are equal. For this reason, ORBDDs are specialy useful to check equivalence of combinational circuits.

## Code Examples

Since the project is in its beginnings, there is not much in this section.

But just for a taste, the following code checks the equivalence of 2 circuits using the first algorithm from 
[this paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.60.5265).

```Haskell
module TestEquiv where

import VerParser
import Algo
import Kuelmann97

import System.Environment
import Equivalence
import Data.Either

main :: IO ()
main = do
          [f1, f2] <- getArgs
          p1 <- parseVerilog f1
          p2 <- parseVerilog f2
          case rights [p1, p2] of
            [r1, r2] -> print $ equiv equivKuelmann97 r1 r2
            _ -> error $ show $ lefts [p1, p2]

```

## Motivation

This is a quote from the Problem B description:

> Efficient equivalence checking and functional correction on large-scale designs are crucial technologies for handling today’s demanding design cycles. Analyzing an entire design is not always practical for large-scale designs because of their size and complexity. It is well known that partitioning the design, based on two designs’ correspondence, can significantly reduce the complexity of the analysis. 
> In this contest, we challenge contestants to insert cut points on large designs in an effort to reduce their equivalence checking and function change problems. 
> 
> [...]
> 
> Identifying corresponding cuts is an effective method of simplifying the equivalence checking problem caused by the Boolean complexity of large-scale designs. The corresponding cuts represent points of functional correspondence between the two designs, allowing you to partition the designs into smaller sub-problems. However, finding the equivalent correspondence can be difficult when the design has gone through ODC/SDC optimization. An algorithm that can effectively determine functional correspondence, can help improve the success of large-scale equivalence checking. 
>
> [...]

In other words, testing circuits is not easy and we need more tools to do it.

## Installation

This projects aims to reduce the amount of depencies to the most. 
The only dependencies are the haskell platform (GHC 7.10) and the modules ***fgl***, ***parsec*** and ***fgl-visualize*** from cabal.
Thes can be istalled by running 

> cabal install fgl parsec fgl-visualize

Later the source code can be compiled using.

> ghc -O2 Main.hs

The equivalence of 2 circuits can then be checked by running:

> ./Main circuit1.v circuit2.v

## API Reference

Since API is not yet defined, it is not wise yet to publish it.

## Tests

Automated test can be executed by running the following line:

> cabal test

This will try to check the equivalence of the tests contained in the ***tests*** folder, obtained from the Problem B of the ICCAD Contest 2015.

Currently, circuits are being checked by 2 methods: 

1. The first one is random simulation using Icarus Verilog. Unfortunately Icarus cannot handle
the verilogs from units3 to unit9.

2. The second is performed by Kuelmann 97. 

| Test      | iverilog | Kuelmann 97 |
|:----------|---------:|------------:|
| BDD       | Yes      | Yes         |
| BDD_wrong | No       | No          |
| unit1     | Yes      | N/A         |
| unit2     | No       | N/A         |
| unit3     | Fail     | N/A         |
| unit4     | Fail     | N/A         |
| unit5     | Fail     | N/A         |
| unit7     | Fail     | N/A         |
| unit8     | Fail     | N/A         |
| unit9     | Fail     | N/A         |
| unit10    | Yes      | N/A         |
| unit11    | No       | N/A         |
| unit12    | Yes      | N/A         |
| unit13    | No       | N/A         |
| unit14    | Yes      | N/A         |
| unit15    | No       | N/A         |
| unit16    | Yes      | N/A         |
| unit17    | No       | N/A         |

Legend: 
* Yes: The circuits are equivalent.
* No: The circuits are not equivalent.
* Fail: The tool fails when checking this input.
* N/A: The tool was not well tested with this input.

## Contributors

For now, only me. But I welcome other contributors!

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
