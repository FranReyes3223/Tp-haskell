module Main where
import           Data.Map           (Map, empty, fromList, insert, lookup, (!))
import           System.Environment (getArgs)
import Prelude hiding (lookup)

leerArchivo :: FilePath -> IO (Int, Int, Int, [[Int]])
leerArchivo archivo = do
  contenido <- readFile archivo
  let (primeraLinea:resto) = lines contenido
      [n, m, k] = map read $ words primeraLinea
      listas = map (map read . words) $ take n resto
  return (n, m, k, listas)

escribirArchivo :: FilePath -> [Int] -> IO ()
escribirArchivo archivo costos = do
  writeFile archivo $ unwords (map show costos)

--   Convierte una lista de listas de enteros (matriz) en un Map donde cada clave es una
--   tupla (i, j) que representa la posición en la grilla, y el valor es el entero correspondiente.
--   Por ejemplo, la posición (0,0) corresponde al primer elemento de la primera fila,
--   (0,1) al segundo elemento de la primera fila, (1,0) al primer elemento de la segunda fila, etc.
--   Esto permite acceder rápidamente al valor de cualquier celda usando su posición.
listasAGrilla :: Int -> Int -> [[Int]] -> Map (Int, Int) Int
listasAGrilla n m lista = go 0 0 lista empty
  where
    -- Recorre cada fila y columna, agregando cada elemento al Map con su posición (i, j)
    go _ _ [] acc = acc
    go i _ (fila:restoFilas) acc = go (i+1) 0 restoFilas (agregarFila i 0 fila acc)
    -- Recorre una fila, agregando cada elemento al Map
    agregarFila _ _ [] acc = acc
    agregarFila i j (x:xs) acc = agregarFila i (j+1) xs (insert (i, j) x acc)

--calcula todos los caminos sin repetir los ya calculados
--usando programacion dinamica(memoizacion)
todosLosCaminos :: Int -> Int -> Int -> Map (Int, Int) Int -> [[(Int, Int)]]
todosLosCaminos n m k grilla = take k $ snd $ caminosKMejores 0 0 empty
  where
    caminosKMejores x y memo
      | x == n-1 && y == m-1 = (memo, [[(x, y)]])
      | x >= n || y >= m = (memo, [])
      | otherwise = 
          case lookup (x, y) memo of --verifica si ya calculamos los caminos para una posicion (x,y)
            Just caminos -> (memo, caminos) --camino ya calculado: no hace cambios
            Nothing ->
              --calculamos los mejores caminos en caso de ir para la izquierda y en caso de ir a la derecha
              let (memo1, caminosAbajo) = caminosKMejores (x+1) y memo
                  (memo2, caminosDerecha) = caminosKMejores x (y+1) memo1

                  --juntamos los caminos calculados al ir a la derecha y los caminos calculados al ir a la izquierda
                  todosCaminos = map ((x, y):) (caminosAbajo ++ caminosDerecha)

                  --guarda los caminos ya calculados con su correspondiente costo
                  caminosConCosto = [ (costoCamino grilla cam, cam) | cam <- todosCaminos ]

                  --guarda los caminos ordenados segun su costo calculado previamente
                  caminosOrdenados = map snd $ ordenarPorCosto caminosConCosto

                  --toma los k mejores caminos
                  --(take toma los primeros k elementos de una lista, y al estar la lista ordenada toma los mejores)
                  kMejores = take k caminosOrdenados

                  --actualiza la memoria con solo los mejores caminos
                  memoFinal = insert (x, y) kMejores memo2
              in (memoFinal, kMejores)


--ordena la lista de caminos de menor a mayor segun el costo
ordenarPorCosto :: [(Int, [(Int, Int)])] -> [(Int, [(Int, Int)])]
ordenarPorCosto = foldr insertarOrdenado []
  where
    insertarOrdenado x [] = [x]
    insertarOrdenado x (y:ys) =
      if fst x <= fst y
      then x : y : ys
      else y : insertarOrdenado x ys


-- Calcula el costo de un camino usando el operador !(lookup).
-- usamos lista por compresion para comprimir la sintaxis y que sea mas legible
costoCamino :: Map (Int, Int) Int -> [(Int, Int)] -> Int
costoCamino grilla camino = sum [ grilla ! pos | pos <- camino ]

--es el encargado de devolver los costos de los k caminos para que se printeen en salida.out
costosMinimos :: Int -> Int -> Int -> Map (Int, Int) Int -> [Int]
costosMinimos n m k grilla =
  let caminos = todosLosCaminos n m k grilla
  in map (costoCamino grilla) caminos

main :: IO ()
main = do
  archivo <- head <$> getArgs
  (n, m, k, listas) <- leerArchivo archivo
  let grilla = listasAGrilla n m listas
  let kMejoresCostos = costosMinimos n m k grilla
  escribirArchivo "salida.out" kMejoresCostos