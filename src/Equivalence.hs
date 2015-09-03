module Equivalence where

import BDD
import Verilog


equiv :: Verilog Int -> Verilog Int -> Bool
equiv _ _ = error "Not implemented"

--equivWire (Wire s1)  (Wire s2)  = s1 == s2
--equivWire (Value v1) (Value v2) = v1 == v2
--equivWire _ _                   = False

--equivNodes g1 n1 g2 n2          =


--equiv g1 g2 = case (labNodes g1, labNodes g2) of
--    (n1, n2) ->
