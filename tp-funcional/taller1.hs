import Test.HUnit

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving (Eq, Show) 
-- instance Show a => Show (AB a) where
-- show t = padAB t 0 0

-- -- Funciones auxiliares

-- pad :: Int -> String
-- pad i = replicate i ' '

-- padAB :: Show a => AB a -> Int -> Int -> String
-- padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- -- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\r i d -> i ++ (r:d))

-- -- Ejercicios
-- EJ 1
recAB :: b -> (a -> AB a -> AB a -> b -> b -> b) -> AB a -> b
recAB z f Nil = z
recAB z f (Bin i v d) = f v i d (recAB z f i) (recAB z f d)  

foldAB :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB z f = recAB z (\v _ _ recI recD -> f v recI recD)

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

-- EJ 5
altura :: AB a -> Int
altura = foldAB 0 (\_ recI recD -> 1 + max recI recD)

cantidadDeNodos :: AB a -> Int
cantidadDeNodos = foldAB 0 (\_ recI recD -> 1 + recI + recD)

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
insertarABB = recAB (\nodoAInsertar -> abHoja nodoAInsertar) (\raiz abIzq abDer recI recD -> (\nodoAInsertar -> if nodoAInsertar <= raiz then Bin (recI nodoAInsertar) raiz abDer else Bin abIzq raiz (recD nodoAInsertar)))

insertarHeap :: Ord a => (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap f Nil e = abHoja e
insertarHeap f ab e = recAB (\nodoAInsertar -> abHoja nodoAInsertar) (\raiz abIzq abDer recI recD -> (\nodoAInsertar -> if completarRamaDerecha abIzq abDer then 
                                                                                                                          if f nodoAInsertar raiz then (Bin abIzq nodoAInsertar (recD raiz))  
                                                                                                                          else Bin abIzq raiz (recD nodoAInsertar) 
                                                                                                                        else 
                                                                                                                          if f nodoAInsertar raiz then (Bin (recI raiz) nodoAInsertar abDer)
                                                                                                                          else Bin (recI nodoAInsertar) raiz abDer)) ab e
                  where completarRamaDerecha izq der = completo izq && (altura izq > altura der)

truncar :: AB a -> Int -> AB a
truncar = foldAB (const Nil) (\v recI recD -> (\nivelDeCorte -> if nivelDeCorte == 0 then Nil else Bin (recI (nivelDeCorte - 1)) v (recD (nivelDeCorte - 1))))

-- ESTRUCTURAS PARA TESTS --

-- -- Heap (<) completo
heapMenor1completo = Bin (abHoja 4) 2 (abHoja 5)
-- -- Heap (<) completo
heapMenor2completo = Bin (abHoja 6) 3 (abHoja 7)
-- -- Heap (>) completo
heapMayor1completo = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- -- Heap (<)
heapMenor3 = Bin heapMenor1completo 1 (abHoja 3)
-- ABB completo
abbCompleto = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- -- Heap (<)
heapMenor4 = Bin heapMenor1completo 0 (abHoja 6)
-- -- ABB
abbIncompleto1 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)
-- -- Heap (<) infinito, probar truncando
heapMenorInfinito = Bin (mapAB (*2) heapMenorInfinito) 1 (mapAB ((+1) . (*2)) heapMenorInfinito)
-- 
abbDegeneradoAIzq = Bin (abHoja 1) 2 Nil
-- 
abbDegeneradoADer = Bin Nil 4 (abHoja 5)
--
abbConIzqYDerNilEnAlgunMomento = Bin abbDegeneradoAIzq 3 abbDegeneradoADer
-- 
heapMayorDegeneradoAIzq = Bin (abHoja 1) 2 Nil
-- 
heapMayorDegeneradoADer = Bin Nil 2 (abHoja 1) -- ojo porque no seria un heap "valido" segun el orden de agregado

-- ejemplos para testear rec y fold
sumaRecursiva :: Num a => AB a -> a
sumaRecursiva Nil = 0
sumaRecursiva (Bin izq v der) = sumaRecursiva izq + v + sumaRecursiva der

sumaRec :: Num a => AB a -> a
sumaRec = recAB 0 (\v _ _ recI recD -> v + recI + recD) 

sumaFold :: Num a => AB a -> a
sumaFold = foldAB 0 (\v recI recD -> v + recI + recD) 

--EJECUCIÓN DE LOS TESTS --
main :: IO Counts
main = do runTestTT allTests

allTests = test ["ejercicio1" ~: testsEj1,
                 "ejercicio2" ~: testsEj2,
                 "ejercicio3" ~: testsEj3,
                 "ejercicio4" ~: testsEj4,
                 "ejercicio5" ~: testsEj5,
                 "ejercicio6" ~: testsEj6,
                 "ejercicio7" ~: testsEj7]

testsEj1 = test [
    [1,2,4,5,7] ~=? inorder abbIncompleto1,
    [1,2,3,4,5,6,7] ~=? inorder abbCompleto,
    sumaRecursiva abbIncompleto1 ~=? sumaRec abbIncompleto1,
    sumaRecursiva abbIncompleto1 ~=? sumaFold abbIncompleto1,
    sumaRecursiva Nil ~=? sumaRec Nil,
    sumaRecursiva Nil ~=? sumaFold Nil,
    sumaRecursiva abbConIzqYDerNilEnAlgunMomento ~=? sumaRec abbConIzqYDerNilEnAlgunMomento,
    sumaRecursiva abbConIzqYDerNilEnAlgunMomento ~=? sumaFold abbConIzqYDerNilEnAlgunMomento
  ]
  
testsEj2 = test [
    [5,3,6,1,7] ~=? inorder (mapAB (+1) heapMenor4),
    [] ~=? inorder (mapAB (+1) Nil),
    [7] ~=? inorder (mapAB (+1) (abHoja 6)),
    [2,3] ~=? inorder (mapAB (+1) abbDegeneradoAIzq),
    [5,6] ~=? inorder (mapAB (+1) abbDegeneradoADer)
  ]

testsEj3 = test [
    True ~=? nilOCumple (<) 1 Nil,
    True ~=? nilOCumple (<) 1 (abHoja 2),
    False ~=? nilOCumple (>=) 1 (abHoja 2)
  ]

testsEj4 = test [
    -- esABB
    -- fue necesario definirlo como izq de abHoja porque al usar Nil podia ser de muchos tipos de Ord y no podia definir cual
    True ~=? esABB (izq (abHoja 2)), --esABB Nil
    True ~=? esABB (abHoja 2),
    True ~=? esABB abbCompleto,
    True ~=? esABB abbConIzqYDerNilEnAlgunMomento,
    False ~=? esABB heapMenor1completo,
    False ~=? esABB heapMayor1completo,

    -- esHeap
    True ~=? esHeap (<) (izq (abHoja 2)), --esHeap Nil
    True ~=? esHeap (<) (abHoja 2),
    True ~=? esHeap (<) heapMenor1completo,
    True ~=? esHeap (>) heapMayor1completo,
    True ~=? esHeap (>) heapMayorDegeneradoAIzq,  
    True ~=? esHeap (>) heapMayorDegeneradoADer,

    False ~=? esHeap (<) heapMayor1completo,
    False ~=? esHeap (<) abbIncompleto1,
    False ~=? esHeap (>) abbIncompleto1,
    False ~=? esHeap (<) heapMayorDegeneradoAIzq,  
    False ~=? esHeap (<) heapMayorDegeneradoADer
  ]

testsEj5 = test [
    True ~=? completo Nil,
    True ~=? completo (abHoja 1),
    True ~=? completo abbCompleto,
    True ~=? completo heapMenor1completo,
    True ~=? completo heapMayor1completo,
    False ~=? completo heapMenor4,
    False ~=? completo abbDegeneradoAIzq,
    False ~=? completo abbDegeneradoADer
  ]

testsEj6 = test [
    -- insertarABB
    True ~=? esABB (insertarABB (insertarABB abbIncompleto1 6) 9),
    True ~=? esABB (insertarABB Nil 6),
    True ~=? esABB (insertarABB (abHoja 3) 6),
    True ~=? esABB (insertarABB (abHoja 3) 1),
    True ~=? esABB (insertarABB abbCompleto 10),
    True ~=? esABB (insertarABB abbIncompleto1 10),

    -- insertarHeap
    True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) heapMenor4 3) 1),
    True ~=? esHeap (<) (insertarHeap (<) Nil 1),

    True ~=? esHeap (<) (insertarHeap (<) (abHoja 2) 1),
    altura (abHoja 2) + 1 ~=? altura (insertarHeap (<) (abHoja 2) 1),

    True ~=? esHeap (>) (insertarHeap (>) (heapMayorDegeneradoAIzq) 8),
    altura heapMayorDegeneradoAIzq ~=? altura (insertarHeap (>) (heapMayorDegeneradoAIzq) 8),

    True ~=? esHeap (>) (insertarHeap (>) (heapMayor1completo) 10),
    altura heapMayor1completo + 1 ~=? altura (insertarHeap (>) (heapMayor1completo) 10)
  ]

testsEj7 = test [
    [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar heapMenorInfinito 4),
    True ~=? esHeap (<) (truncar heapMenorInfinito 5),

    altura Nil ~=? altura (truncar Nil 2),

    [] ~=? inorder (truncar (abHoja 5) 0),
    [] ~=? inorder (truncar heapMayor1completo 0),

    [5] ~=? inorder (truncar (abHoja 5) 1),
    [7] ~=? inorder (truncar heapMayor1completo 1),

    [2,3,4] ~=? inorder (truncar (Bin abbDegeneradoAIzq 3 (abHoja 4)) 2),
    [2,3,4] ~=? inorder (truncar (Bin (abHoja 2) 3 abbDegeneradoADer) 2)
  ]

-- para correr reload y main en un solo comando
-- :def run const $ return $ unlines [":reload",":main"]
-- luego se llama a :run