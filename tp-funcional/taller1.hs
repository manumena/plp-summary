--import Test.HUnit

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving (Eq, Show) 
-- instance Show a => Show (AB a) where
-- show t = padAB t 0 0

-- -- Funciones auxiliares

--pad :: Int -> String
--pad i = replicate i ' '

--padAB :: Show a => AB a -> Int -> Int -> String
-- padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- -- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
-- inorder :: AB a -> [a]    
-- inorder = foldAB [] (\i r d -> i ++ (r:d))

-- Estructuras para tests

-- -- Heap (<) completo
-- ab1 = Bin (abHoja 4) 2 (abHoja 5)
-- -- Heap (<) completo
-- ab2 = Bin (abHoja 6) 3 (abHoja 7)
-- -- Heap (>) completo
-- ab3 = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- -- Heap (<)
-- ab4 = Bin ab1 1 (abHoja 3)
-- -- ABB completo
-- ab5 = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- -- Heap (<)
-- ab6 = Bin ab1 0 (abHoja 6)
-- -- ABB
-- ab7 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)
-- -- Heap (<) infinito, probar truncando
-- ab8 = Bin (mapAB (*2) ab8) 1 (mapAB ((+1) . (*2)) ab8)

-- -- Ejercicios
-- EJ 1
recAB :: b -> (a -> AB a -> AB a -> b -> b -> b) -> AB a -> b
recAB z f Nil = z
recAB z f (Bin i v d) = f v i d (recAB z f i) (recAB z f d)  

foldAB :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB z f ab = recAB z (\v i d recI recD -> f v recI recD) ab

-- EJ 2
mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\v recI recD -> Bin recI (f v) recD)

-- EJ 3
nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple f e Nil = True
nilOCumple f e (Bin i v d) = f e v

-- EJ 4
esABB :: Ord a => AB a -> Bool
esABB = recAB True (\v abI abD recI recD -> recI && recD && ((comparar v abI (>)) && (comparar v abD (<))))
        where comparar v ab fcomp = foldAB True (\raiz recI recD -> (fcomp v raiz) && recI && recD) ab

esHeap :: (a -> a -> Bool) -> AB a ->  Bool
esHeap fcomp = recAB True (\v abI abD recI recD -> recI && recD && (comparar v abI) && (comparar v abD))
        where comparar v ab = foldAB True (\raiz recI recD -> (fcomp v raiz) && recI && recD) ab


-- ejemplos de uso de rec y fold
sumaRec = recAB 0 (\v _ _ recI recD -> v + recI + recD) 
sumaFold = foldAB 0 (\v recI recD -> v + recI + recD) 
-- 

-- EJ 5
altura :: AB a -> Int
altura = foldAB 0 (\v recI recD -> 1 + max recI recD)

cantidadDeNodos :: AB a -> Int
cantidadDeNodos = foldAB 0 (\v recI recD -> 1 + recI + recD)

completo :: AB a -> Bool
completo ab = (2^altura ab) - 1 == cantidadDeNodos ab

-- EJ 6
raiz :: AB a -> a
raiz (Bin i v d) = v

izq :: AB a -> AB a
izq Nil = Nil
izq (Bin i v d) = i

der :: AB a -> AB a
der Nil = Nil
der (Bin i v d) = d

esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB Nil e = abHoja e
insertarABB ab e = insertarABBAux e (raiz ab) ab

insertarABBAux :: Ord a => a -> a -> AB a -> AB a
insertarABBAux e r = foldAB Nil (\v recI recD -> if (v<r && e<r) || (v>r && e>r) then insertar v recI recD else (Bin recI v recD))
    where insertar v recI recD = (Bin (if e <= v && recI == Nil then (abHoja e) else recI) v (if e > v && recD == Nil then (abHoja e) else recD))


insertarHeap :: Ord a => (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap f Nil e = abHoja e
insertarHeap f ab e = heapify f (insertarHeapAux ab e)

insertarHeapAux :: Ord a => AB a -> a -> AB a
insertarHeapAux ab e = recAB (abHoja e) (\v abIzq abDer recI recD -> if tengoQueMeterADerecha abIzq abDer then (Bin abIzq v recD) else (Bin recI v abDer)) ab
    where tengoQueMeterADerecha izq der = completo izq && (altura izq > altura der)

heapify :: (a -> a -> Bool) -> AB a -> AB a
heapify f = foldAB Nil (\v recI recD -> if giroConIzq v recI then (Bin (Bin (izq recI) v (der recI)) (raiz recI) recD) else 
    if giroConDer v recD then (Bin recI (raiz recD) (Bin (izq recD) v (der recD))) else (Bin recI v recD))
    where giroConIzq v recI = (not (esNil recI)) && (f (raiz recI) v)
          giroConDer v recD = (not (esNil recD)) && (f (raiz recD) v)

truncar :: AB a -> Int -> AB a
truncar ab = foldAB (const Nil) (\v recI recD -> (\nivelDeCorte -> if nivelDeCorte == 0 then Nil else Bin (recI (nivelDeCorte - 1)) v (recD (nivelDeCorte - 1)))) ab


--if hayQueCortar v abIzq abDer then Nil else (Bin recI v recD)
--where hayQueCortar v abIzq abDer = (max (altura abIzq) (altura abDer)) + 1 == alturaCorte  

-- --Ejecución de los tests
-- main :: IO Counts
-- main = do runTestTT allTests

-- allTests = test [
--   "ejercicio1" ~: testsEj1,
--   "ejercicio2" ~: testsEj2,
--   "ejercicio3" ~: testsEj3,
--   "ejercicio4" ~: testsEj4,
--   "ejercicio5" ~: testsEj5,
--   "ejercicio6" ~: testsEj6,
--   "ejercicio7" ~: testsEj7
--   ]

-- testsEj1 = test [
--   [1,2,4,5,7] ~=? inorder ab7,
--   [1,2,3,4,5,6,7] ~=? inorder ab5
--   ]
  
-- testsEj2 = test [
--   [5,3,6,1,7] ~=? inorder (mapAB (+1) ab6)
--   ]

-- testsEj3 = test [
--   0 ~=? 0 --Cambiar esto por tests verdaderos.
--   ]

-- testsEj4 = test [
--   0 ~=? 0 --Cambiar esto por tests verdaderos.
--   ]

-- testsEj5 = test [
--   0 ~=? 0 --Cambiar esto por tests verdaderos.
--   ]

-- testsEj6 = test [
--   True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
--   True ~=? esABB (insertarABB (insertarABB ab7 6) 9)
--   ]

-- testsEj7 = test [
--   [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
--   True ~=? esHeap (<) (truncar ab8 5)
--   ]
