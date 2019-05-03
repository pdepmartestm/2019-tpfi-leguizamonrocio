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