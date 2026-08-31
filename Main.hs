module Main where
import Tablero
import Solver

main :: IO ()
main = do
    entrada <- getLine
    let n = read entrada :: Int
    let tablero = crearTablero n
    let resultado = quienGana X tablero
    case resultado of
        GanaX -> putStrLn "X"
        GanaO -> putStrLn "O"
        Empate -> putStrLn "E"