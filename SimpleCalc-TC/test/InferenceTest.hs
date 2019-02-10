module InferenceTest (tests) where
    import SCStructures as SCS
    import Inference
    import Test.Tasty
    import Test.Tasty.HUnit

    tests = [test1,test2,test3, test4,test5]

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