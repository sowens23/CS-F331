import System.IO

inputLength = do
  putStr "Type some text: "
  hFlush stdout
  line <- getLine
  putStr ""
  putStr "You typed: "
  putStrLn line
  putStr "Length of your line = "
  putStrLn $ show $ length line

myGetLine :: IO String
myGetLine = do
  c <- getChar
  if c == '\n'
    then ????
    else do 
      rest <- myGetLinereturn (c:rest)

squareNums :: IO ()
squareNums = do
  putStr "Type an integer (0 to quit): "
  -- Flush between I/O actions
  hFlush stdout
  line <- getLine
  let n = read line
  if n == 0 
    then return ()
    else do 
      putStrLn "
      putStr "The square of your number is "
      putStrLn $ show $ n*n
      putStrLn ""
      squareNums

main = squareNums
data Product = Pr String String
-- product name, manufacturer name

doNothing :: Product -> Product
doNothing (Pr pn mn) = Pr pn mn

pName :: Product -> String
pName (Pr pn _) = pn

mName :: Product -> String
mName (Pr _ mn) = mn

sameProduct :: Product -> Product -> Bool
sameProduct (Pr pn1 mn1) (Pr pn2 mn2) = 
  (pn1 == pn2) && (mn1 == mn2)

-- Instance declaration (Product is an instance of Eq)
intsance Eq Product where
  Pr pn1 mn1 == Pr pn2 mn2 =
    (pn1 == pn2) && (mn1 == mn2)

-- Write a show for our function
-- Ex. Frosted Flakes [made by Kellogs]
instance show Product where
  show (Pr pn mn) = pn ++ " [made by " ++ mn ++ "]"
