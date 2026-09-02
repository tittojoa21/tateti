module Reglas where 

import Tablero 

filas :: Tablero -> [[Char]] 

filas tablero = tablero 

columnas :: Tablero -> [[Char]] 

columnas tablero = [[tablero !! fila !! columna | fila <- [0 .. n - 1]] | columna <- [0 .. n - 1]]
    where 
        n = length tablero

principalDiagonal :: Tablero -> [Char] 

principalDiagonal tablero = [tablero !! i !! i | i <- [0 .. n - 1]]
    where
        n = length tablero

secundariaDiagonal :: Tablero -> [Char] 

secundariaDiagonal tablero = [tablero !! i !! (n - 1 - i) | i <- [0 .. n - 1]]
    where
        n = length tablero

lineas :: Tablero -> [[Char]] 

lineas tablero = filas tablero ++ columnas tablero ++ [principalDiagonal tablero] ++ [secundariaDiagonal tablero]

gano :: Tablero -> Char -> Bool

gano tablero jugador = any (all (== jugador)) (lineas tablero)

vacias :: Tablero -> Bool

vacias tablero = any (== ' ') (concat tablero) -- Junta todas las filas y busca algún espacio vacío.

juegoTerminado :: Tablero -> Bool -- Recibe un tablero y determina si el juego terminó.

juegoTerminado tablero = gano tablero 'X' || gano tablero 'O' || not (vacias tablero) -- Termina si gana X, gana O o no quedan casillas.
