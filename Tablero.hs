module Tablero where -- Define el módulo que representa la estructura y operaciones del tablero del juego.

type Tablero = [[Char]] -- Define un tablero como una matriz de caracteres, donde cada celda guarda 'X', 'O' o espacio vacío.

crearTablero :: Int -> Tablero -- Crea un tablero vacío de tamaño n x n.
crearTablero n = replicate n (replicate n ' ') -- Genera n filas, cada una con n caracteres vacíos.

jugadas :: Tablero -> [(Int, Int)] -- Devuelve todas las posiciones libres del tablero.
jugadas tablero = [(fila, columna) | fila <- [0 .. n - 1], columna <- [0 .. n - 1], tablero !! fila !! columna == ' '] -- Recorre todas las casillas y selecciona solo las vacías.
    where
        n = length tablero -- Calcula la dimensión del tablero para saber cuánto recorrer.

jugar :: Tablero -> Char -> (Int, Int) -> Tablero -- Aplica un movimiento en una posición específica del tablero.
jugar tablero jugador (fila, columna) = take fila tablero ++ [nuevaFila] ++ drop (fila + 1) tablero -- Reemplaza la fila indicada con una versión actualizada.
    where
        viejaFila = tablero !! fila -- Obtiene la fila original donde se va a colocar el símbolo.
        nuevaFila = take columna viejaFila ++ [jugador] ++ drop (columna + 1) viejaFila -- Inserta el símbolo en la columna especificada y deja el resto de la fila intacto.
