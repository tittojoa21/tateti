module Reglas where -- Define el módulo que contiene las reglas del juego.

import Tablero -- Importa el tipo de datos y construcciones del tablero.

filas :: Tablero -> [[Char]] -- Define la función que devuelve las filas del tablero.

filas tablero = tablero -- La representación del tablero ya está organizada por filas, así que se devuelve tal cual.

columnas :: Tablero -> [[Char]] -- Define la función que obtiene cada columna del tablero.

columnas tablero = [[tablero !! fila !! columna | fila <- [0 .. n - 1]] | columna <- [0 .. n - 1]] -- Para cada columna, recoge los elementos de esa posición en todas las filas.
    where -- Inicia la sección local donde se calculan variables auxiliares.
        n = length tablero -- Guarda la cantidad de filas del tablero para recorrer índices válidos.

principalDiagonal :: Tablero -> [Char] -- Define la diagonal principal del tablero.

principalDiagonal tablero = [tablero !! i !! i | i <- [0 .. n - 1]] -- Recorre los índices y toma la posición (i, i) en cada fila.
    where
        n = length tablero -- Determina el tamaño del tablero para acotar los índices.

secundariaDiagonal :: Tablero -> [Char] -- Define la diagonal secundaria del tablero.

secundariaDiagonal tablero = [tablero !! i !! (n - 1 - i) | i <- [0 .. n - 1]] -- Recorre la diagonal inversa tomando la posición (i, n-1-i).
    where
        n = length tablero -- Calcula el tamaño del tablero para invertir la diagonal correctamente.

lineas :: Tablero -> [[Char]] -- Define una lista con todas las líneas que pueden ganar en el juego.

lineas tablero = filas tablero ++ columnas tablero ++ [principalDiagonal tablero] ++ [secundariaDiagonal tablero] -- Combina filas, columnas y diagonales en una sola lista.

gano :: Tablero -> Char -> Bool -- Evalúa si un jugador tiene una línea completa.

gano tablero jugador = any (all (== jugador)) (lineas tablero) -- Comprueba si alguna línea está formada completamente por el símbolo del jugador.

vacias :: Tablero -> Bool -- Determina si aún hay espacios vacíos en el tablero.

vacias tablero = any (== ' ') (concat tablero) -- Junta todas las filas y busca al menos un espacio libre.

juegoTerminado :: Tablero -> Bool -- Recibe un tablero y determina si la partida terminó.

juegoTerminado tablero = gano tablero 'X' || gano tablero 'O' || not (vacias tablero) -- La partida termina si gana X, gana O o ya no quedan casillas libres.
