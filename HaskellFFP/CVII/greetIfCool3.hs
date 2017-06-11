module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    case cool of
        True  -> putStrLn "eyyyyy. What's shakin'?"
        False -> putStrLn "pshhhh."
        where cool = coolness == "downright frosty yo"

----------------------------------------
functionC x y = if (x > y) then x else y

functionC' x y = case x > y of
    True  -> x
    False -> y
