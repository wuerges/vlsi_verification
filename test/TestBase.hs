module TestBase where

filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]
files = filesCorrect ++ filesWrong
complete f = ["tests/"++f++"/in_1.v", "tests/"++f++"/in_2.v"]
fileNames = concat [complete f | f <- files]

