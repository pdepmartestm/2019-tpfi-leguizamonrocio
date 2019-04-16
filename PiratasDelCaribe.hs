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

