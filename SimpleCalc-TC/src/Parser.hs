module Parser where
    import Control.Applicative
    import Data.Char
    import SCStructures

    newtype Parser a = P (String -> [(a,String)])

    parse :: Parser a -> String -> [(a,String)]
    parse (P parser) inp = parser inp

    item :: Parser Char
    item = P( \inp -> case inp of 
                    [] -> []
                    (x:xs) -> [(x,xs)]) 

    instance Functor Parser where 
        -- fmap :: (a->b) -> Parser a -> Parser b
        fmap f p = P (\inp -> case (parse p inp) of 
                            [] -> []
                            [(a,rest)] -> [(f a, rest)]  
                            ) 

    instance Applicative Parser where 
        --pure :: a -> Parser a 
        pure f = P (\inp -> [(f, inp)])
        -- <*> :: Parser (a->b) -> Parser a -> Parser b
        funp <*> pa = P (\inp -> case (parse funp inp) of
                                [] -> []
                                [(f, rest)] -> parse (fmap f pa) rest                         
            )

    instance Monad Parser where 
        -- >>= :: Parser a -> (a -> Parser b) -> Parser b
        pa >>= f = P (\inp -> case parse pa inp of 
                                [] -> []
                                [(a,rest)] -> parse (f a) rest )


    instance Alternative Parser where
        --empty :: Parser a 
        empty = P( \inp -> [])
        -- <|> :: Parser a -> Parser a -> Parser a
        p <|> p' = P ( \inp -> case parse p inp of 
                                [] -> parse p' inp 
                                [(a,rest)] -> [(a,rest)])
        
    sat :: (Char -> Bool) -> Parser Char
    sat p = item >>= \x -> if p x then return x else empty

    digit :: Parser Char 
    digit = sat isDigit 

    lower :: Parser Char 
    lower = sat isLower

    upper :: Parser Char 
    upper = sat isUpper

    letter :: Parser Char 
    letter = sat isAlpha

    alphanum :: Parser Char 
    alphanum = sat isAlphaNum

    char :: Char -> Parser Char
    char c = sat (==c)

    string :: String -> Parser String
    string [] = return []
    string (x:xs) = char x >>= \x' ->
                    string xs >>= \xs' ->
                        return (x:xs) 

    keywords :: [String]
    keywords = ["false","true","if","else","then","fn"]

    isKeyWord :: String -> Bool 
    isKeyWord s = elem s keywords

    nat :: Parser Int
    nat = some digit >>= \x ->
            return (read x)
        
    space :: Parser ()
    space = many (sat isSpace) >>= \x->return ()

    token ::Parser a -> Parser a 
    token p = space >>= \s ->
            p >>= \t ->
            space >>= \s' ->
            return t

    natural :: Parser Int
    natural = token nat

    var :: Parser String
    var = do l <- letter
             ls <- (many (alphanum <|> (char '_')))
             if (isKeyWord (l:ls)) then empty
             else return (l:ls)
    
    variable :: Parser String
    variable = token (var)

    symbol :: String -> Parser String
    symbol xs = token (string xs)
    
------------------------------------------
    parseBType :: Parser Type --tipi base
    parseBType = 
        fmap (\_->TNat) (symbol "Nat") <|>
        fmap (\_->TBool) (symbol "Bool") 

    parseType :: Parser Type
    parseType = pure(\x arr y -> TArrow x y) <*> parseBType <*> symbol "->" <*> parseType <|>
                parseBType
-------------------------------------------
                


    parseTerm :: Parser (Term)
    parseTerm = 
        parseApp 

    parseBTerm :: Parser (Term)
    parseBTerm = 
        parseSum <|>
        parseMin <|>
        parseCond <|>
        parseFn <|>
        parseATerm

    parseApp :: Parser Term
    parseApp = (some parseBTerm) >>= \xs -> return (listOfAp xs)
                
    listOfAp :: [Term] -> Term
    listOfAp (x:[]) = x
    listOfAp xs = EAp (listOfAp (init xs)) (last xs)
    ---------------------------------------------      
    parseATerm :: Parser Term 
    parseATerm = 
        parseVar  <|>
        parseNum <|>
        parseBool <|>
        pure(\ _ x _ -> x) <*> symbol "(" <*> parseTerm <*> symbol ")"

    parseVar :: Parser Term
    parseVar = fmap EVar variable

    parseNum :: Parser Term
    parseNum = fmap ENum natural

    parseBool :: Parser Term
    parseBool = fmap (\_->EBool True) (symbol "true") <|>
                fmap (\_->EBool False) (symbol "false")
    --------------------------------------------
    parseSum :: Parser Term
    parseSum = pure(\x op y-> ESum x y) <*> parseATerm <*> symbol "+" <*> parseBTerm
                

    parseMin :: Parser Term 
    parseMin = pure(\x op y->EMin x y) <*> parseATerm <*> symbol "-" <*> parseBTerm

    parseFn :: Parser Term 
    parseFn = 
        do 
            symbol "fn"
            x <- variable
            symbol ":"
            t <- parseType
            symbol "."
            m <- parseBTerm
            return (Efn x t m)
        
    parseCond :: Parser Term
    parseCond = 
        do 
            symbol "if"
            m1 <- parseTerm
            symbol "then"
            m2 <- parseTerm
            symbol "else"
            m3 <- parseTerm
            return (ECond m1 m2 m3)


            
            