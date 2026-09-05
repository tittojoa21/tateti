module Solver where -- Define el módulo encargado de resolver el juego y analizar los posibles resultados.

import Tablero -- Importa el tipo Tablero y las funciones relacionadas con el mismo.

import Reglas -- Importa las reglas del juego, como la detección de líneas y espacios vacíos.

data Jugador = X | O deriving (Show, Eq) -- Define los dos jugadores posibles del juego.

data Resultado = GanaX | GanaO | Empate deriving (Show, Eq) -- Define los posibles resultados finales de una partida.

otro :: Jugador -> Jugador -- Devuelve el jugador contrario al recibido.

otro X = O -- Si el jugador actual es X, el oponente es O.

otro O = X -- Si el jugador actual es O, el oponente es X.

marca :: Jugador -> Char -- Traduce un jugador a su símbolo en el tablero.

marca X = 'X' -- El jugador X usa la ficha X.

marca O = 'O' -- El jugador O usa la ficha O.

resultadoJugador :: Jugador -> Resultado -- Asocia cada jugador con el resultado que representa en la lógica del juego.

resultadoJugador X = GanaX -- Si el jugador es X, el resultado favorable es que X gane.

resultadoJugador O = GanaO -- Si el jugador es O, el resultado favorable es que O gane.

siguientes :: Tablero -> Jugador -> [Tablero] -- Genera todos los tableros posibles después de que el jugador haga un movimiento.

siguientes tablero jugador = [jugar tablero (marca jugador) posicion | posicion <- jugadas tablero] -- Para cada casilla libre, crea el tablero resultante de marcar esa posición.

quienGana :: Jugador -> Tablero -> Resultado -- Determina el mejor resultado posible desde la posición actual para el jugador dado.

quienGana jugador tablero
    | gano tablero 'X' = GanaX -- Si X ya ganó, el resultado es que X gane.
    | gano tablero 'O' = GanaO -- Si O ya ganó, el resultado es que O gane.
    | not (vacias tablero) = Empate -- Si no quedan casillas y nadie ganó, el juego termina en empate.
    | otherwise = mejorResultado jugador resultados -- Si sigue en curso, evalúa todas las opciones y selecciona el mejor resultado.
    where
        estados = siguientes tablero jugador -- Genera los tableros inmediatos después de cada movimiento posible.
        resultados = [quienGana (otro jugador) estado | estado <- estados] -- Recurre sobre cada posible siguiente tablero para analizar el resultado.

mejorResultado :: Jugador -> [Resultado] -> Resultado -- Elige el resultado óptimo para el jugador según las posibilidades.

mejorResultado jugador resultados
    | resultadoJugador jugador `elem` resultados = resultadoJugador jugador -- Si el jugador puede forzar una victoria, se devuelve esa condición.
    | Empate `elem` resultados = Empate -- Si existe al menos un empate, se toma como resultado seguro.
    | otherwise = resultadoJugador (otro jugador) -- Si no se puede ganar ni empatar, se asume la derrota del jugador actual.
