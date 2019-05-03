import Text.Show.Functions 
import Data.List 
import Data.Maybe 

type Nombre = String
type Tesoros = (String, Int)
type FormaDeSaqueo = Tesoros -> Bool

data Pirata = Pirata {
    nombre :: Nombre, 
    botin :: [Tesoros]
} deriving (Show, Eq)

--Algunos Piratas
jackSparrow = Pirata "Jack Sparrow" [("Brujula que apunta", 10000), ("Frasco de arena", 0)]
davidJones = Pirata "David Jones" [("Cajita musical", 1)]
anneBonny = Pirata "Anne Bonny" [("Doblones", 100), ("Frasco de arena", 1)]

--TESOROS PIRATAS
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

--TEMPORADA DE SAQUEOS
saquearTesorosValiosos :: Tesoros -> Bool
saquearTesorosValiosos tesoro = tesoroValioso tesoro 

saquearObjetoEspecifico :: Tesoros -> Tesoros -> Bool
saquearObjetoEspecifico objeto tesoro = objeto == tesoro

saquearConCorazon :: Tesoros -> Bool 
saquearConCorazon tesoro = False

saquearFormaCompleja :: Tesoros -> Tesoros -> Bool
saquearFormaCompleja objeto tesoro = saquearObjetoEspecifico objeto tesoro || saquearTesorosValiosos tesoro

saquear :: Pirata -> FormaDeSaqueo -> Tesoros -> Pirata
saquear pirata formaDeSaqueo tesoro = pirata {botin = (botin pirata) ++ (filter formaDeSaqueo [tesoro])}

--Probando diferentes combinaciones

-- *Main> saquear anneBonny (saquearObjetoEspecifico ("Oro", 100)) ("Oro", 100)
-- Pirata {nombre = "Anne Bonny", botin = [("Doblones",100),("Frasco de arena",1),("Oro",100)]}

-- *Main> saquear davidJones (saquearConCorazon) ("Oro", 100)
-- Pirata {nombre = "David Jones", botin = [("Cajita musical",1)]}

-- *Main> saquear jackSparrow (saquearFormaCompleja ("Sombrero", 10)) ("Oro", 100)
-- Pirata {nombre = "Jack Sparrow", botin = [("Brujula que apunta",10000),("Frasco de arena",0)]}
