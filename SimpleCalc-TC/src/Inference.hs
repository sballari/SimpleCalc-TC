module Inference where 
    import SCStructures
    import STPattern

    type Contest = [(VName, Type)] -- Contest

    typeOfExe :: Term -> Maybe Type
    typeOfExe t = fst (app (typeOf [] t) [])

    enrichError :: String -> ST (Maybe Type)
    enrichError err = ST(\e-> (Nothing,e++[err]))

    typeOf :: Contest -> Term -> ST (Maybe Type)
    typeOf ctx (EVar x) =
            let mtx = lookup x ctx in      
            if mtx == Nothing then enrichError ("var "++x++" not in the context") 
            else return mtx    
            
    typeOf ctx (Efn x s term) = do  mt <- typeOf ((x,s):ctx) term 
                                    case mt of 
                                        Nothing -> return Nothing
                                        Just t -> return (Just (TArrow s t))
    typeOf ctx (EAp m1 m2) = 
        do 
            mt1 <- typeOf ctx m1 
            mt2 <- typeOf ctx m2
            case mt1 of 
                Nothing -> return Nothing
                Just (TArrow t11 t12) -> case mt2 of 
                                            Nothing -> return Nothing
                                            Just t2 -> if t2 <= t11     
                                                        then return (Just t12)
                                                        else enrichError ("parametrer type mismatch")  
                _ -> enrichError("arrow type expected")
    
    typeOf ctx (ECond bexp m1 m2) = 
        do 
            mb <- typeOf ctx bexp
            mt1 <- typeOf ctx m1
            mt2 <- typeOf ctx m2
            case mb of
                Just b ->   if b<=TBool then
                                case mt1 of 
                                    Nothing -> enrichError ("first branch typing error") 
                                    Just t1 -> case mt2 of 
                                                    Nothing -> enrichError ("second branch typing error") 
                                                    Just t2 -> let tif = lub t1 t2 in
                                                                if tif == Nothing
                                                                    then enrichError ("impossible joint of if stm") 
                                                                    else return tif
                            else enrichError("a correct boolean guard has not been provided") 
                Nothing -> enrichError("impossible to type the guard") 


    typeOf ctx (ESum m1 m2) = 
        do 
            mt1 <- typeOf ctx m1
            mt2 <- typeOf ctx m2 
            case pure(\x y -> x <= TNat && y <=TNat) <*> mt1 <*> mt2 of 
                Nothing -> enrichError ("impossible to type a operator of the sum")   
                Just True -> return (Just TNat)
                Just False -> enrichError ("one of the operator is not a subtype of TNat")
                
    typeOf ctx (EMin m1 m2) = 
        do 
            mt1 <- typeOf ctx m1
            mt2 <- typeOf ctx m2 
            case pure(\x y -> x <= TNat && y <=TNat) <*> mt1 <*> mt2 of 
                Nothing -> enrichError ("impossible to type a operator of the sum")   
                Just True -> return (Just TNat)
                Just False -> enrichError ("one of the operator is not a subtype of TNat")

    typeOf ctx (EBool b) = return (Just TBool)
    typeOf ctx (ENum n) = return (Just TNat)

    

    



