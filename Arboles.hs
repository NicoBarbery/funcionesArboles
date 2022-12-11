data Direccion = Norte
               | Sur
			   | Este
			   | Oeste
			deriving Show


nombreDireccion :: Direccion -> String
nombreDireccion Norte = "N"
nombreDireccion Sur   = "S"
nombreDireccion Oeste = "O"
nombreDireccion Este  = "E"

data Point = Pt Float Float
           deriving Show


origen = Pt 0 0

punto = Pt 5.6 7.2



norm :: Point -> Float
norm (Pt x y) = sqrt (x*x + y*y)


zipPoint :: [Float] -> [Float] -> [Point]
zipPoint xs ys = map (uncurry Pt) (zip xs ys)

-- Definiciones alternativas
--zipPoint xs ys = map (\(x,y) -> Pt x y) (zip xs ys)
-- = [Pt x y | (x, y) <- zip xs ys]

data Shape = Rectangle Point Float Float
           | Circle Point Float
           | Triangle Point Point Point


retan = Rectangle origen 5 6

circ = Circle punto 6.5

tria = Triangle origen punto (Pt 2.5 2.5)


perimetro :: Shape -> Float
perimetro (Rectangle (Pt x y) ancho alto) = 2 * (ancho + alto)
perimetro (Circle pt radio)               = 2 * pi * radio
perimetro (Triangle pt1 pt2 pt3)          = distancia pt1 pt2
                                          + distancia pt2 pt3
										  + distancia pt3 pt1


distancia :: Point -> Point -> Float
distancia (Pt x1 y1) (Pt x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)


--- Tipos de Datos Recursivos

data IntList = EmptyList
             | Cons Int IntList
			 deriving Show

-- [1,2,3]
lista = Cons 1 (Cons 2 (Cons 3 EmptyList))

long :: IntList -> Int
long (EmptyList) = 0
long (Cons _ ls) = 1 + long ls

-- Arboles de Enteros

data IntTree = EmptyTree
             | Node Int IntTree IntTree
			 deriving Show

arb1 = Node 4 (Node 6 EmptyTree (Node 7 EmptyTree EmptyTree)) EmptyTree

arb2 = Node 42 (Node 1 EmptyTree EmptyTree) EmptyTree

elemTree :: Int -> IntTree -> Bool

elemTree _ EmptyTree = False
elemTree n (Node x ai ad) | n == x = True
                          | otherwise = elemTree n ai || elemTree n ad


treeHeight :: IntTree -> Int
treeHeight EmptyTree = 0
treeHeight (Node x ai ad) = 1 + max (treeHeight ai) (treeHeight ad)

treeCount :: IntTree -> Int
treeCount EmptyTree = 0
treeCount (Node x ai ad) = 1 * treeCount ai + treeCount ad

generalTree = (Int -> a -> a -> a) -> a -> IntTree -> a
generalTree _ v EmptyTree      = v
generalTree f v (Node x ai ad) = f x (foldTree f v ai) (foldTree f v ad)

treeHeight' = foldTree (/x ri ri rd -> 1 + max ri rd) 0

treeCount'  = foldTree (x ri rd -> 1 + ri + rd) 0

treeSum :: IntTree -> Int
--treeSum

--de un arbol a una lista
toIntList :: IntTree -> IntList
toIntList EmptyTree -> EmptyList
toIntList (Node x ai ad) = Cons x (toIntList ai <++> toIntList ad)

<++> :: IntList -> IntList
EmptyList <++> ys = ys
(Cons x xs) <++> ys = Cons x (xs <++> ys)

--Funcion General

toIntList' = foldTree (/x r1 r2 -> Cons x (r1 <++> r2) EmptyList
