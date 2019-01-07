module ParserTest (tests) where
    import Parser
    import SCStructures as SCS
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [test1,test2,test3,test4,test5,test6,test7]

    resultP :: Parser a -> String -> a
    resultP p i = (\[(x,y)]->x) (parse p i)

    test1  = testCase "parse var1" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "var1"
            expected = EVar "var1"

    test2  = testCase "parse false" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "false"
            expected = EBool False
    
    test3  = testCase "parse false+3" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "false+3"
            expected = ESum (EBool False) (ENum 3)

    test4  = testCase "parse false 3 x" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "false 3 x"
            expected = EAp (EAp (EBool False) (ENum 3) )  (EVar "x")

    test5  = testCase "parse false+4 3" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "false + 4  3"
            expected = EAp (ESum (EBool False) (ENum 4) )  (ENum 3)

    test6  = testCase "parse if 3 then 3 else 3" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "if 3 then 3 else 3"
            expected = ECond (ENum 3) (ENum 3) (ENum 3) 

    test7  = testCase "fn x:Nat -> Nat.x 3" (assertEqual "" expected result)
        where 
            result = resultP parseTerm "fn x:Nat -> Nat.x 3"
            expected = EAp (Efn "x" (TArrow TNat TNat) (EVar "x")) (ENum 3)