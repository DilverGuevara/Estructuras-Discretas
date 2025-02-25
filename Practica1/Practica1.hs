


abcisa :: Float -> Float -> Float
abcisa p q = p + (-q)
ordenada :: Float -> Float -> Float
ordenada r s = r + (-s)
distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (w, x) (y, z) = sqrt ((abcisa w y)^2 + (ordenada x z)^2)



pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2,y2) = if y1 == y2
    then 0
    else (y2 - y1) / (x2 - x1)



valorAbsoluto :: Int -> Int
valorAbsoluto z = if z < 0
        then -z
        else z




hipotenusa:: Float -> Float -> Float
hipotenusa b h = sqrt (b^2 + h^2)



raices:: Float -> Float -> Float -> (Float, Float)
raices a b c = ((((-b) + sqrt (b^2 - (4*a*c))) / (2*a)) , ((-b) - sqrt (b^2 - (4*a*c))/ (2*a)))



semiperimetro:: Float -> Float -> Float -> Float
semiperimetro d e f = (d+e+f)/2
areaTriangulo:: Float -> Float -> Float -> Float
areaTriangulo i j k = sqrt ((semiperimetro i j k)*((semiperimetro i j k) - i)*((semiperimetro i j k)-j)*((semiperimetro i j k)-k))



esBisiesto:: Int -> Bool
esBisiesto ye = (mod ye 4 == 0) && (mod ye 100 /= 0)
    


comparador:: Int -> Int -> Int
comparador n1 n2 = if n1 < n2
    then -1
    else if n1 > n2
        then 1
        else 0



maximo:: Int -> Int -> Int -> Int
maximo z1 z2 z3 = if z1>=z2 && z1>z3
    then z1
    else if z2 >= z1 && z2>z3
        then z2
        else z3



esDescendente:: Int -> Int -> Int -> Int -> Bool
esDescendente k1 k2 k3 k4 = (k1-k2)*(k2-k3)*(k3-k4)>0
