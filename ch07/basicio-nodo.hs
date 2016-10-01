-- Basic IO - Sequencing
--
main = do
       putStrLn "Greetings! What is your name?" >>
         getLine >>=
         (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")