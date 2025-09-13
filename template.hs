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

listasAGrilla :: Int -> Int -> [[Int]] -> Map (Int, Int) Int
listasAGrilla n m lista = go 0 0 lista empty
  where
    go _ _ [] acc = acc
    go i _ (fila:restoFilas) acc = go (i+1) 0 restoFilas (agregarFila i 0 fila acc)
    agregarFila _ _ [] acc = acc
    agregarFila i j (x:xs) acc = agregarFila i (j+1) xs (insert (i, j) x acc)

todosLosCaminos :: Int -> Int -> Int -> Map (Int, Int) Int -> [[(Int, Int)]]
todosLosCaminos n m k grilla = take k $ snd $ caminosKMejores 0 0 empty
  where
    caminosKMejores x y memo
      | x == n-1 && y == m-1 = (memo, [[(x, y)]])
      | x >= n || y >= m = (memo, [])
      | otherwise =
          case lookup (x, y) memo of
            Just caminos -> (memo, caminos)
            Nothing ->
              let (memo1, caminosAbajo) = caminosKMejores (x+1) y memo
                  (memo2, caminosDerecha) = caminosKMejores x (y+1) memo1
                  todosCaminos = map ((x, y):) (caminosAbajo ++ caminosDerecha)
                  caminosConCosto = map (\cam -> (costoCamino grilla cam, cam)) todosCaminos
                  caminosOrdenados = map snd $ ordenarPorCosto caminosConCosto
                  kMejores = take k caminosOrdenados
                  memoFinal = insert (x, y) kMejores memo2
              in (memoFinal, kMejores)

ordenarPorCosto :: [(Int, [(Int, Int)])] -> [(Int, [(Int, Int)])]
ordenarPorCosto = foldr insertarOrdenado []
  where
    insertarOrdenado x [] = [x]
    insertarOrdenado x (y:ys) =
      if fst x <= fst y
      then x : y : ys
      else y : insertarOrdenado x ys

costoCamino :: Map (Int, Int) Int -> [(Int, Int)] -> Int
costoCamino grilla camino = sum [ grilla ! pos | pos <- camino ]

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