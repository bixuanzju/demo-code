import Data.Vect


-- Example 1 
-- Every programming language begins with HelloWorld example
main : IO ()
main = putStrLn "Hello world"






































-- Example 2: Calculating a type

StringOrInt : Bool -> Type
StringOrInt b = case b of
                  True => String
                  False => Int


getStringOrInt : (singleton : Bool) -> StringOrInt singleton
getStringOrInt singleton = case singleton of
                             True =>  "hello"
                             False => 42


valToString : (singleton : Bool) -> StringOrInt singleton -> String
valToString singleton v = case singleton of
                           True => v
                           False => cast v













































-- Example 3: Type, define, refine
-- Calculating word lengths

-- +-------------+        +------+
-- |             |        |      |
-- | "Hong"      |        | 4    |
-- --------------+        -------+
-- |             |        |      |
-- | "Kong"      |        | 4    |
-- |             |        |      |
-- --------------+        -------+
-- |             |        |      |
-- | "University"|        | 10   |
-- |             |        |      |
-- +-------------+        +------+

word_lengths : Vect n String -> Vect n Nat
word_lengths [] = []
word_lengths (x :: xs) = length x :: word_lengths xs








































-- Example 4: Type-directed search

zipWith2 : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith2 f [] [] = []
zipWith2 f (x :: xs) (y :: ys) = f x y :: zipWith2 f xs ys





















-- Example 5: Implicit arguments
-- appendV2 with explicit arguments











-- Find the length of a vector
lengthV : Vect n elem -> Nat
lengthV {n} xs = n










































-- Example 6: Sort a vector

insert : Ord elem => (x : elem) -> (xs_sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x <= y of
                          False => y :: insert x xs
                          True => x :: y :: xs

ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
                     insert x xs_sorted









































-- Example 7: Transposing a matrix


-- +--     --+                +--  --+
-- | 1  2  3 |                | 1  4 |
-- |         |  transpose to  |      |
-- | 4  5  6 |                | 2  5 |
-- |         |                |      |
-- +--     --+                | 3  6 |
--                            +--  --+

create_empty : Vect n (Vect 0 elem)
create_empty {n = Z} = []
create_empty {n = (S k)} = [] :: create_empty

xTranspose : (x : Vect n elem) -> (xs_tranpose : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
xTranspose [] [] = []
xTranspose (x :: xs) (y :: ys) = (x :: y) :: xTranspose xs ys

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empty
transpose_mat (x :: xs) = let xs_tranpose = transpose_mat xs in
                          xTranspose x xs_tranpose



Point : Type
Point = (Double, Double)

Polygon : Nat -> Type
Polygon  = \ n => Vect n Point



































-- Example 8:

-- data Fin : Nat -> Type where
--   FZ : Fin (S k)
--   FS : Fin k -> Fin (S k)


-- index : Fin n -> Vect n a -> a

n2F : (m : Nat) -> (n : Nat) -> Maybe (Fin n)
n2F Z Z = Nothing
n2F Z (S k) = Just FZ
n2F (S k) Z = Nothing
n2F (S k) (S j) = case n2F k j of
                    Just n' => Just (FS n')
                    Nothing => Nothing

i2F : Integer -> (n : Nat) -> Maybe (Fin n)
i2F x n = if x >= 0 then (n2F (cast x) n) else Nothing


tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case i2F x n of
                      Just n' => Just (index n' xs)
                      Nothing => Nothing











































-- Example 9: A type safe printf function

-- A few examples:

-- printf "Hello" : String
-- printf "Age is: %d" : Int -> String
-- printf "%s has %d dogs" : String -> Int -> String

data Format = Number Format
            | Str Format
            | Lit String Format
            | Cha Format
            | Dou Format
            | End

PrintfType : Format -> Type
PrintfType (Number x) = Int -> PrintfType x
PrintfType (Str x) = String -> PrintfType x
PrintfType (Cha x) = Char -> PrintfType x
PrintfType (Dou x) = Double -> PrintfType x
PrintfType (Lit x y) = PrintfType y
PrintfType End = String


printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \n => printfFmt x (acc ++ show n)
printfFmt (Str x) acc = \s => printfFmt x (acc ++ s)
printfFmt (Cha x) acc = \c => printfFmt x (acc ++ cast c)
printfFmt (Dou x) acc = \d => printfFmt x (acc ++ show d)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Cha (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dou (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                          Lit lit chars' => Lit (strCons c lit) chars'
                          fmt => Lit (cast c) fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""


-- Efficiency of printfFmt?
