import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment       
import System.IO
import System.IO.Error
import Data.List
import Data.Time
import Matriz  

main = try `catchIOError` handler

try :: IO ()
try = do         
    start <- getCurrentTime

    (fileA:fileB:fileC:bx:by:_) <- getArgs
    handler1 <- openFile fileA ReadMode
    handler2 <- openFile fileB ReadMode
    matriz1 <- hGetContents handler1
    putStrLn matriz1
    matriz2 <- hGetContents handler2
    putStrLn matriz2
    let
    	a = Mat (parseInt(((words matriz1)!!1))) (parseInt(((words matriz1)!!0))) (genMatList matriz1)
    	b = Mat (parseInt(((words matriz2)!!1))) (parseInt(((words matriz2)!!0))) (genMatList matriz2)
        c = matMulBlock (parseInt by) (parseInt bx) a b `using` (strategia rseq)

    writeFile fileC ("xd")
    putStrLn $ "Matriz C en Lista: " ++ show(c)

    hClose handler1
    hClose handler2

    stop <- getCurrentTime
    print $ diffUTCTime stop start



handler :: IOError -> IO()   
handler e = putStrLn "El archivo no existe!"

parseInt :: String -> Int
parseInt "" = 0
parseInt text =  read text :: Int

toInt :: Float -> Int
toInt x = round x

genMatList' :: String -> [String]
genMatList' "" = []
genMatList' text = drop 2 (lines (text))

genMatList'' :: [String] -> [[Int]]
genMatList'' [] = [[]]
genMatList'' x = map (\l -> map read (words l)) x

genMatList :: String -> [[Int]]
genMatList "" = []
genMatList text = genMatList'' (genMatList' (text))

--Da la oportunidad al usuario de poder escoger la Strategia de paralelismo
strategia :: Strategy a -> Strategy a
strategia sa = strategia' (rparWith sa)

-- Asigna una Strategia para lograr el paralelismo
strategia' :: Strategy a -> Strategy a
strategia' sa a = do
    a' <- sa a
    return a'
