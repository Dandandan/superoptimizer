{-# LANGUAGE NegativeLiterals #-}
import Data.Int
import Data.Bits
import qualified Data.Set as Set
import Data.List (find, intercalate)

data Instruction
    = Push Int8
    | Add
    | Sub
    | Mul
    | And
    | Or
    | Lds
    deriving (Eq, Show)

type Program = [Instruction]

eval :: Program -> [Int8]
eval [] = []
eval (Push i: xs) = i: eval xs
eval (Add: xs) =
    let
        x:y:z = eval xs
    in
        x + y: z
eval (Sub: xs) =
    let
        x:y:z = eval xs
    in
        x - y: z
eval (Mul: xs) =
    let
        x:y:z = eval xs
    in
        x * y: z
eval (And: xs) =
    let
        x:y:z = eval xs
    in
        x .&. y: z
eval (Or: xs) =
    let
        x:y:z = eval xs
    in
        (x .|. y): z
eval (Lds: xs) =
    let 
        x:y = eval xs
    in
        x:x:y

type Trace = [Int8]
type UniqueTraces = Set.Set Trace

createTrace :: Program -> Trace
createTrace p = map (\i -> head $ eval (p ++ [Push i])) [(-128)..127]

binary :: [Instruction]
binary = [Add, Sub, Mul, And, Or]

vars :: [Instruction]
vars = map Push [(-1)..1] ++ [Lds]

addOne :: Instruction -> Program -> [Program]
addOne i [] = [[i]]
addOne i p@(x:xs) = (i:p): map (x:) (addOne i xs)

expand :: Program -> [Program]
expand p = concatMap (\b -> map (b:) (concatMap (\c -> addOne c p) vars)) binary

uniqueExpansions :: UniqueTraces -> [Program] -> ([Program], UniqueTraces)
uniqueExpansions unique [] = ([], unique)
uniqueExpansions unique (e:xs) =
    let trace = createTrace e
        notMem = Set.notMember trace unique 
        newUnique = Set.insert trace unique
        (res, lastUnique) = uniqueExpansions newUnique xs
    in
        (if notMem then e:res else res, lastUnique)

allPrograms :: UniqueTraces -> [Program] -> [Program]
allPrograms unique prev =
    let expanded = concatMap expand prev
        (uniqueExp, newUnique) = uniqueExpansions unique expanded
    in
        uniqueExp ++ allPrograms newUnique uniqueExp

computeAll :: [Program]
computeAll = allPrograms (Set.fromList [createTrace []]) ([[]])

findSmallest :: Trace -> Maybe Program
findSmallest t = find (\p -> createTrace p == t) computeAll
        
findExample = findSmallest ((map (\i -> i ^ 4 + 1)) [(-128)..127]::[Int8])


printProgram :: Program -> String
printProgram program = 
    let
        trace = map show $ createTrace program

    in
        intercalate ","  trace ++ ":"  ++ show program

main :: IO ()
main = 
    mapM_ (putStrLn . printProgram) computeAll

