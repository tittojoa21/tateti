module Main where -- Define el módulo principal del programa de tatetí.
import Tablero -- Importa las definiciones y funciones del tablero.
import Solver -- Importa la lógica para evaluar el resultado de una partida.

main :: IO () -- Declara la función principal que ejecuta la aplicación.
main = do -- Inicia la secuencia de acciones del programa.
    entrada <- getLine -- Lee la entrada del usuario desde la consola.
    let n = read entrada :: Int -- Convierte la cadena leída en un número entero que representa el tamaño del tablero.
    let tablero = crearTablero n -- Crea un tablero vacío de tamaño n x n.
    let resultado = quienGana X tablero -- Evalúa quién gana desde la perspectiva del jugador X en el tablero actual.
    case resultado of -- Decide la salida según el resultado calculado.
        GanaX -> putStrLn "X" -- Si el ganador es X, imprime X en consola.
        GanaO -> putStrLn "O" -- Si el ganador es O, imprime O en consola.
        Empate -> putStrLn "E" -- Si hay empate, imprime E en consola.