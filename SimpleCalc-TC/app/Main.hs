module Main where

import Lib
import Inference
import Parser
import STPattern

main :: IO ()
main = 
    do
         putStrLn "TYPE INFERENCE"
         putStrLn "enter the program followed by a end in a new line"
         programStr <- mainloop 
         case (parse parseTerm programStr) of
            [] -> putStrLn "ERROR: parsing error"
            [(program,rest)] -> 
                if rest /= "" then print "ERROR: text non completely parsed"
                else 
                    let (mt,errors) = app (typeOf [] program) [] in
                    putStrLn ("Program tree: "++(show program)++
                    case mt of 
                        Nothing -> "\nERROR: inference incomplete\n"++(printErrors errors)
                        Just t -> "\nINFERED TYPE: "++(show t))
            

mainloop :: (IO String)
mainloop = 
    do
        inpStr <- getLine
        if inpStr == "end"
            then return []
            else do 
                    lines <- mainloop
                    return (inpStr ++ lines)

printErrors :: Error -> String 
printErrors errs = (foldr (\(n,err) sr -> (show n)++") "++err++"\n" ) "") (zip [1..] errs)