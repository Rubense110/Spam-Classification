-- ---------------------------------------------------------------------
-- arbol binario: 

--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)

-- Donde la H representa que es una Hoja, y la N es un nodo interior

-- Por ejemplo, el árbol

--         9 
--        / \
--       /   \
--      3     7
--     / \  
--    2   4 

-- se representa por

--    N 9 (N 3 (H 2) (H 4)) (H 7) 
-- ---------------------------------------------------------------------
-- tendremos nuestro arbol de la forma: 

--            feature
--             valor
--          /         \
--         <=          >
--         /           \
--       feature       class
--        valor
--      /       \  
--     <=        >
--    /           \  
--  class       class 

-- donde segun el valor se tomará una rama u otra a la hora de navegarlo, sin embargo dado que siempre será el <= a la izda y el > a la dcha lo podemos representar en haskell así:

--            feature
--             valor
--          /         \
--         /           \
--       feature       class
--        valor
--      /       \  
--     /         \  
--  class       class 

-- tendremos una dupla para los nodos y valores para las hojas, representando estas las clases.
-- 87-y+CB5QU/9bxd
-- ---------------------------------------------------------------------

module DecissionTree
    (Feature,
     Class,
     DecissionTree (..),
     nHojas
    ) where


-- tipo sinónimo para la caracteristica del nodo, junto a su valor de decision 
type Feature = (Int, Float)

-- tipo sinónimo para la clase final al llegar a una hoja
type Class = Int

-- Deficinicion del tipo de dato arbol de decision
data DecissionTree = L Class
                   | N Feature DecissionTree, DecissionTree
                   deriving (Show, Eq)

                   -- N 9 (N 3 (H 2) (H 4)) (H 7) 

-- funcion para contar hojas
nHojas :: DecissionTree -> Int
nHojas (L _)     = 1
nHojas (N _ (i,d)) = nHojas i + nHojas d

--a = N (3,0.8) ((L 2),(L 1)) 