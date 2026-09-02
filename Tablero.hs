module Tablero where

type Tablero = [[Char]]

crearTablero :: Int -> Tablero
crearTablero n = replicate n (replicate n ' ')

jugadas :: Tablero -> [(Int, Int)] 
jugadas tablero = [(fila, columna) | fila <- [0 .. n - 1], columna <- [0 .. n - 1], tablero !! fila !! columna == ' '] 
    where
        n = length tablero

jugar :: Tablero -> Char -> (Int, Int) -> Tablero
jugar tablero jugador (fila, columna) = take fila tablero ++ [nuevaFila] ++ drop (fila + 1) tablero 
    where
        viejaFila = tablero !! fila
        nuevaFila = take columna viejaFila ++ [jugador] ++ drop (columna + 1) viejaFila
