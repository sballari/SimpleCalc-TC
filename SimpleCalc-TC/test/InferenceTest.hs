module InferenceTest (tests) where
    import SCStructures as SCS
    import Inference
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [test1,test2,test3, test4,test5,test6,test7,test8,test9,test10]

    test1  = testCase "Inf1: sum" (assertEqual "" expected result)
        where 
            expected = Just TNat
            result = typeOfExe (ESum (ENum 3)(ENum 3))

    test2  = testCase "Inf2: if false 3 4" (assertEqual "" expected result)
        where 
            expected = Just TNat
            result = typeOfExe (ECond (EBool False)(ENum 3)(ENum 3))

    test3  = testCase "Inf3: (fn x:TNat x+3) 4 " (assertEqual "" expected result)
        where 
            expected = Just TNat
            result = typeOfExe (EAp (Efn "x" TNat (ESum (EVar "x")(ENum 3))) (ENum 4))
    
    test4 = testCase "Inf4: x+3" (assertEqual "" expected result)
        where
            expected = Nothing
            result = typeOfExe ( ESum (EVar "x") (ENum 3))

    test5  = testCase "Inf5: if 4 true 4" (assertEqual "" expected result)
        where 
            expected = Just TBool
            result = typeOfExe (ECond (ENum 4)(EBool True)(ENum 4))
    
    test6  = testCase "Inf6: if (fn x:Bool .x) 3 4" (assertEqual "" expected result)
        where 
            expected = Nothing
            result = typeOfExe (ECond (Efn "x" TBool (EVar "x")) (ENum 3) (ENum 3))

    test7  = testCase "Inf7: if ((fn x:Bool .x) true) true 4" (assertEqual "" expected result)
        where 
            expected = Just TBool
            result = typeOfExe (ECond (EAp (Efn "x" TBool (EVar "x")) (EBool True)) (ENum 3) (EBool False))

    test8  = testCase "Inf8: if ((fn x:Bool .x) true) 3 4" (assertEqual "" expected result)
        where 
            expected = Just TNat
            result = typeOfExe (ECond (EAp (Efn "x" TBool (EVar "x")) (EBool True)) (ENum 3) (ENum 6))

    test9  = testCase "Inf9: if true then fn x:Bool.3 else fn x:Nat.true" (assertEqual "" expected result)
        where 
            expected = Just (TArrow TNat TBool)
            result = typeOfExe (ECond (EBool True) (Efn "x" TBool (ENum 3)) (Efn "x" TNat (EBool True)))

    test10  = testCase "Inf10: if 4 then (fn x:Nat->Nat.(x 3))  else fn x:Nat->Nat .false" (assertEqual "" expected result)
        where 
            expected = Just (TArrow (TArrow TNat TNat) TBool)
            result = typeOfExe (ECond (ENum 3) ((Efn "x" (TArrow TNat TNat) (EAp (EVar "x") (ENum 3)))) ( (Efn "x" (TArrow TNat TNat) (EBool False))))
        
            