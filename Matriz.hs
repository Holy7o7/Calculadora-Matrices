module Matriz where
--se crea el tipo de dato matrix (no se usara lectura ni escritura de archivos)
data Matriz a = Mat Int Int [[a]] deriving Show 

--funcion para obtener la dimension n de una matrix
getN :: Matriz a -> Int
getN (Mat n m mat) = n

--funcion para obtener la dimension m de una matrix
getM :: Matriz a -> Int
getM (Mat n m mat) = m

--Funcion que obtiene la posicion de los elementos de una matrix
pos :: Matriz a -> Int -> Int -> a
pos (Mat n m mat) i j = mat !! i !! j

--Funcion de multiplicacion normal de 2 matrices
matMul :: Matriz Int -> Matriz Int -> Matriz Int
matMul a b = Mat m n [[sum [(pos a i q) * (pos b q j) | q <- [0..k-1]] | j <- [0..n-1]] | i <- [0..m-1]]
	where
		m = getN a
		k = getM a
		n = getM b

--Funcion que suma 2 matrices
matSum :: Matriz Int -> Matriz Int -> Matriz Int
matSum a b = Mat n m [[pos a i j + pos b i j | j <- [0..m-1]] | i <- [0..n-1]]
	where
		n = getN a
		m = getM a

--funcion para obtener un bloque bx * by de una matrix
getSlice :: Int -> Int -> Int -> Int -> Matriz a -> Matriz a
getSlice y x by bx (Mat n m mat) = Mat by bx $ ((take by) . (drop y)) $ map ((take bx) . (drop x)) mat

--Funcion que crea un matrix de los bloques bx * by de una matrix
sliceMat :: Int -> Int -> Matriz a -> Matriz (Matriz a)
sliceMat by bx mat@(Mat n m mat') = Mat (div n by) (div m bx) [[getSlice i j by bx mat | j <- [0, bx .. m-1]] | i <- [0, by .. n-1]]

--Funcion que realiza la multiplicacion de matrices de matrices
matMatMul :: Matriz (Matriz Int) -> Matriz (Matriz Int) -> Matriz (Matriz Int)
matMatMul a b = Mat m n [[foldl1 matSum [matMul (pos a i q) (pos b q j) | q <- [0..k-1]] | j <- [0..n-1]] | i <- [0..m-1]]
	where
		m = getN a
		k = getM a
		n = getM b

--Funcion que obtiene la posicion global de un elemento de una matriz que esta guardado dentro de una matriz de matrices
subpos :: Matriz (Matriz a) -> Int -> Int -> a
subpos mat@(Mat n m mat') i j = pos (pos mat y x) ii jj
	where
		by = getN $ head $ head mat'
		bx = getM $ head $ head mat'
		y = div i by
		ii = mod i by
		x = div j bx
		jj = mod j bx

--convierte una matrix de matrices en una matrix
concatMat :: Matriz (Matriz a) -> Matriz a
concatMat mat@(Mat n m mat') = Mat (n*by) (m*bx) [[subpos mat i j | j <- [0..m*bx-1]] | i <- [0..n*by-1]]
	where
		by = getN $ head $ head mat'
		bx = getM $ head $ head mat'

--Funcion que realiza la multiplicacion de matrices por bloques 
matMulBlock :: Int -> Int -> Matriz Int -> Matriz Int -> Matriz Int
matMulBlock by bx a b = concatMat $ matMatMul mA mB
	where
		mA = sliceMat by bx a
		mB = sliceMat bx by b 

--Funcion que convierte una matriz a la forma de arreglo
matArray :: Matriz a -> [a]
matArray (Mat n m mat) = concat mat

--Funcion que convierte un arreglo a una matriz n * m
arrayMat :: Int -> Int -> [a] -> Matriz a
arrayMat n m mat = Mat m n [[mat!!(i * m + j) | j <- [0..m-1]] | i <- [0..n-1]]

--matrices A y B para pruebas
matA :: Matriz Int
matA = Mat 6 6 [[if i == j then 1 else 0 | i <- [0..5]] | j <- [0..5]]
matB :: Matriz Int
matB = Mat 6 6 [[i * 6 + j | j <- [0..5]] | i <- [0..5]]
