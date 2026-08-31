module Tablero where -- Define el módulo del tablero.

type Tablero = [[Char]] -- Define el tablero como una lista de listas de caracteres.

crearTablero :: Int -> Tablero -- Recibe un entero y devuelve un tablero.
crearTablero n = replicate n (replicate n ' ') -- Crea un tablero de N filas y N columnas vacías.

jugadas :: Tablero -> [(Int, Int)] -- Recibe un tablero y devuelve las posiciones vacías.
jugadas tablero = [(fila, columna) | fila <- [0 .. n - 1], columna <- [0 .. n - 1], tablero !! fila !! columna == ' '] -- Busca todas las casillas vacías.
    where -- Define los valores auxiliares de la función.
        n = length tablero -- Obtiene el tamaño del tablero.

jugar :: Tablero -> Char -> (Int, Int) -> Tablero -- Recibe un tablero, un jugador y una posición.
jugar tablero jugador (fila, columna) = take fila tablero ++ [nuevaFila] ++ drop (fila + 1) tablero -- Reemplaza la fila modificada.
    where -- Define los valores auxiliares de la función.
        viejaFila = tablero !! fila -- Obtiene la fila que vamos a modificar.
        nuevaFila = take columna viejaFila ++ [jugador] ++ drop (columna + 1) viejaFila -- Coloca la marca en la posición indicada.