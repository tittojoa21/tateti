module Reglas where -- Define el módulo que contiene las reglas del juego.

import Tablero -- Importa el tipo Tablero desde el módulo Tablero.

filas :: Tablero -> [[Char]] -- Recibe un tablero y devuelve sus filas.

filas tablero = tablero -- Devuelve las filas del tablero.

columnas :: Tablero -> [[Char]] -- Recibe un tablero y devuelve sus columnas.

columnas tablero = [[tablero !! fila !! columna | fila <- [0 .. n - 1]] | columna <- [0 .. n - 1]] -- Construye las columnas del tablero.
    where -- Define valores auxiliares para esta función.
        n = length tablero -- Obtiene el tamaño del tablero.

principalDiagonal :: Tablero -> [Char] -- Recibe un tablero y devuelve su diagonal principal.

principalDiagonal tablero = [tablero !! i !! i | i <- [0 .. n - 1]] -- Obtiene las posiciones de la diagonal principal.
    where -- Define valores auxiliares para esta función.
        n = length tablero -- Obtiene el tamaño del tablero.

secundariaDiagonal :: Tablero -> [Char] -- Recibe un tablero y devuelve su diagonal secundaria.

secundariaDiagonal tablero = [tablero !! i !! (n - 1 - i) | i <- [0 .. n - 1]] -- Obtiene las posiciones de la diagonal secundaria.
    where -- Define valores auxiliares para esta función.
        n = length tablero -- Obtiene el tamaño del tablero.

lineas :: Tablero -> [[Char]] -- Recibe un tablero y devuelve todas las líneas posibles.

lineas tablero = filas tablero ++ columnas tablero ++ [principalDiagonal tablero] ++ [secundariaDiagonal tablero] -- Junta filas, columnas y diagonales.

gano :: Tablero -> Char -> Bool -- Recibe un tablero y una marca y comprueba si ganó.

gano tablero jugador = any (all (== jugador)) (lineas tablero) -- Comprueba si existe una línea completa con esa marca.

vacias :: Tablero -> Bool -- Recibe un tablero y comprueba si quedan casillas vacías.

vacias tablero = any (== ' ') (concat tablero) -- Junta todas las filas y busca algún espacio vacío.

juegoTerminado :: Tablero -> Bool -- Recibe un tablero y determina si el juego terminó.

juegoTerminado tablero = gano tablero 'X' || gano tablero 'O' || not (vacias tablero) -- Termina si gana X, gana O o no quedan casillas.