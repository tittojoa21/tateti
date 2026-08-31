module Solver where -- Define el módulo que contiene el solver.

import Tablero -- Importa las funciones relacionadas con el tablero.

import Reglas -- Importa las funciones relacionadas con las reglas.

data Jugador = X | O deriving (Show, Eq) -- Define los dos jugadores posibles.

data Resultado = GanaX | GanaO | Empate deriving (Show, Eq) -- Define los tres resultados posibles.

otro :: Jugador -> Jugador -- Recibe un jugador y devuelve el contrario.

otro X = O -- Si juega X, el siguiente jugador es O.

otro O = X -- Si juega O, el siguiente jugador es X.

marca :: Jugador -> Char -- Convierte un jugador en su marca.

marca X = 'X' -- La marca de X es el carácter X.

marca O = 'O' -- La marca de O es el carácter O.

resultadoJugador :: Jugador -> Resultado -- Convierte un jugador en su resultado de victoria.

resultadoJugador X = GanaX -- La victoria de X es GanaX.

resultadoJugador O = GanaO -- La victoria de O es GanaO.

siguientes :: Tablero -> Jugador -> [Tablero] -- Recibe un tablero y un jugador y devuelve los tableros posibles.

siguientes tablero jugador = [jugar tablero (marca jugador) posicion | posicion <- jugadas tablero] -- Realiza todas las jugadas posibles.

quienGana :: Jugador -> Tablero -> Resultado -- Determina el resultado del juego.

quienGana jugador tablero -- Recibe el jugador actual y el tablero.
    | gano tablero 'X' = GanaX -- Si X ya ganó, devuelve GanaX.
    | gano tablero 'O' = GanaO -- Si O ya ganó, devuelve GanaO.
    | not (vacias tablero) = Empate -- Si no quedan casillas, devuelve empate.
    | otherwise = mejorResultado jugador resultados -- Si el juego continúa, analiza las jugadas.
    where -- Define valores auxiliares.
        estados = siguientes tablero jugador -- Genera todos los tableros posibles.
        resultados = [quienGana (otro jugador) estado | estado <- estados] -- Analiza recursivamente cada tablero.

mejorResultado :: Jugador -> [Resultado] -> Resultado -- Elige el mejor resultado para el jugador.

mejorResultado jugador resultados -- Recibe el jugador y los resultados posibles.
    | resultadoJugador jugador `elem` resultados = resultadoJugador jugador -- Si puede ganar, elige ganar.
    | Empate `elem` resultados = Empate -- Si puede empatar, elige empate.
    | otherwise = resultadoJugador (otro jugador) -- Si no puede ganar ni empatar, pierde.