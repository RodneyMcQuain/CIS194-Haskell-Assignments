module TowersOfHanaoi where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discCount peg1 peg2 peg3
  | discCount == 0 = []
  | discCount == 1 = [(peg1, peg2)]
  | discCount > 1 =
    hanoi (discCount - 1) peg1 peg3 peg2 ++ [(peg1, peg2)] ++ hanoi (discCount - 1) peg3 peg2 peg1
