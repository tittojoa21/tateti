module Solver where 

import Tablero 

import Reglas

data Jugador = X | O deriving (Show, Eq) 

data Resultado = GanaX | GanaO | Empate deriving (Show, Eq) 

otro :: Jugador -> Jugador

otro X = O 

otro O = X

marca :: Jugador -> Char

marca X = 'X'

marca O = 'O'

resultadoJugador :: Jugador -> Resultado

resultadoJugador X = GanaX

resultadoJugador O = GanaO

siguientes :: Tablero -> Jugador -> [Tablero]

siguientes tablero jugador = [jugar tablero (marca jugador) posicion | posicion <- jugadas tablero]

quienGana :: Jugador -> Tablero -> Resultado

quienGana jugador tablero
    | gano tablero 'X' = GanaX
    | gano tablero 'O' = GanaO
    | not (vacias tablero) = Empate
    | otherwise = mejorResultado jugador resultados
    where
        estados = siguientes tablero jugador
        resultados = [quienGana (otro jugador) estado | estado <- estados]

mejorResultado :: Jugador -> [Resultado] -> Resultado

mejorResultado jugador resultados
    | resultadoJugador jugador `elem` resultados = resultadoJugador jugador
    | Empate `elem` resultados = Empate
    | otherwise = resultadoJugador (otro jugador)
