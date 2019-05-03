<<<<<<< HEAD
import Text.Show.Functions 
import Data.List 
import Data.Maybe 

type Nombre = String
type Tesoros = (String, Int)

data Pirata = Pirata {
    nombre :: Nombre, 
    botin :: [Tesoros]
} deriving (Show, Eq)

--Algunos Piratas
jackSparrow = Pirata "Jack Sparrow" [("Brujula que apunta", 10000), ("Frasco de arena", 0)]
davidJones = Pirata "David Jones" [("Cajita musical", 1)]
anneBonny = Pirata "Anne Bonny" [("Doblones", 100), ("Frasco de arena", 1)]

--Tesoros Piratas
cantidadTesoros :: Pirata -> Int 
cantidadTesoros = length.botin

pirataAfortunado :: Pirata -> Bool 
pirataAfortunado = (>=10000).valorTotal

valorTotal :: Pirata -> Int
valorTotal = sum.map snd.botin 

mismoNombreValorDiferente :: Pirata -> Pirata -> Bool
mismoNombreValorDiferente pirata1 pirata2 = any (loTiene pirata2) (botin pirata1)

loTiene :: Pirata -> Tesoros -> Bool
loTiene pirata tesoro = any (mismoNombreDistintoValor(tesoro)) (botin pirata)

mismoNombreDistintoValor :: Tesoros -> Tesoros -> Bool
mismoNombreDistintoValor tesoro1 tesoro2 = (fst tesoro1 == fst tesoro2) && (snd tesoro1 /= snd tesoro2)

tesoroMasValioso :: Pirata -> Int
tesoroMasValioso = maximum.obtenerValores

obtenerValores :: Pirata -> [Int]
obtenerValores = map snd.botin

adquiereNuevoTesoro :: Pirata -> Tesoros -> Pirata
adquiereNuevoTesoro pirata tesoro = pirata {botin = (botin pirata) ++ [tesoro]}

pierdeTesorosValiosos :: Pirata -> Pirata
pierdeTesorosValiosos pirata = pirata {botin = (sinTesorosValiosos.botin) pirata}

sinTesorosValiosos :: [Tesoros] -> [Tesoros]
sinTesorosValiosos botin = filter (not.tesoroValioso) botin

tesoroValioso :: Tesoros -> Bool
tesoroValioso tesoro = (snd tesoro) > 100
=======
import Text.Show.Functions 
import Data.List 
import Data.Maybe 

type Nombre = String
type Tesoros = (String, Int)

data Pirata = Pirata {
    nombre :: Nombre, 
    botin :: [Tesoros]
} deriving (Show, Eq)

--Algunos piratas
jackSparrow = Pirata "Jack Sparrow" [("Brujula que apunta", 10000), ("Frasco de arena", 0)]
davidJones = Pirata "David Jones" [("Cajita musical", 1)]
anneBonny = Pirata "Anne Bonny" [("Doblones", 100), ("Frasco de arena", 1)]

--Tesoros piratas
cantidadTesoros :: Pirata -> Int 
cantidadTesoros = length.botin

sumarValorTotal :: Pirata -> Int
sumarValorTotal = sum.map snd.botin

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = sumarValorTotal(pirata) >= 10000

obtenerNombres :: Pirata -> [String]
obtenerNombres = map fst.botin

mismoTesoro :: Pirata -> Pirata -> [String]
mismoTesoro pirata1 pirata2 = intersect (obtenerNombres pirata1) (obtenerNombres pirata2)

obtenerValores :: Pirata -> [Int]
obtenerValores = map snd.botin

tesoroMasValioso :: Pirata -> Int 
tesoroMasValioso = maximum.obtenerValores

nuevoBotin unBotin unPirata = unPirata {
    botin = unBotin
}

nuevosTesoros unTesoro unPirata = nuevoBotin (unTesoro : botin unPirata)

nuevoTesoro = nuevosTesoros

pierdeTesorosValiosos :: Pirata -> [Int]
pierdeTesorosValiosos pirata = filter (>100) (obtenerValores(pirata))

>>>>>>> 4d9fe909307623291465263d11db1ce73d7047e7
