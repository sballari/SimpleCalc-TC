module STPattern where
    type Error = [String]

    newtype ST a = ST (Error -> (a,Error))
    app :: ST a -> Error -> (a,Error)
    app (ST sta) s = sta s

    instance Functor ST where 
        fmap g st = ST (\s-> let (x,s')= app st s in (g x, s'))
    instance Applicative ST where
        pure x = ST (\s -> (x,s))
        stf <*> sta = ST (\s-> 
            let (f,s')= app stf s 
                (x,s'')= app sta s' in (f x , s'') )
    instance Monad ST where
        sta >>= f = ST (\s -> let (a,s') = app sta s in app (f a) s')


    enrichError :: String -> ST ()
    enrichError err = ST(\e-> ((),e++[err]))
