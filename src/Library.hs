module Library where
import PdePreludat

-- 1
type RecursoNatural = String

data Pais = Pais {
    ingresoPerCapita :: Float,
    poblacionPublica :: Int,
    poblacionPrivada :: Int,
    recursosNaturales :: [RecursoNatural],
    deuda :: Float
} deriving (Show)

namibia :: Pais
namibia = Pais {
    ingresoPerCapita = 4140,
    poblacionPublica = 400000,
    poblacionPrivada = 650000,
    recursosNaturales = ["minería", "ecoturismo"],
    deuda = 50000000
}

argentina :: Pais
argentina = Pais {
    ingresoPerCapita = 3000,
    poblacionPublica = 700000,
    poblacionPrivada = 9000000,
    recursosNaturales = ["petroleo", "ecoturismo"],
    deuda = 80000000
}

portugal :: Pais
portugal = Pais {
    ingresoPerCapita = 6000,
    poblacionPublica = 100000,
    poblacionPrivada = 80000,
    recursosNaturales = recursosNaturalesInfinitos,
    deuda = 5000000
}

paises = [ namibia, portugal, argentina]

-- 2

type Receta = Pais -> Pais

porcentaje :: Float -> Float -> Float
porcentaje valor = (* valor) . (/ 100)

interesDeuda :: Float -> Float
interesDeuda = porcentaje 150

prestar :: Float -> Receta
prestar cantidad pais = 
                    pais { 
                        deuda =  (deuda pais) + (interesDeuda cantidad) 
                    } 


perdidaIngresoPerCapita :: Pais -> Int -> Float
perdidaIngresoPerCapita pais cantidad 
    | cantidad > 100 = (porcentaje 20 . ingresoPerCapita) pais 
    | otherwise = (porcentaje 15 . ingresoPerCapita) pais

reducirPoblacionPublica :: Int -> Receta
reducirPoblacionPublica cantidad pais = 
                    pais {
                        poblacionPublica = poblacionPublica pais - cantidad,
                        ingresoPerCapita = ingresoPerCapita pais - perdidaIngresoPerCapita pais cantidad
                    } 

otorgarRecurso :: String -> Receta
otorgarRecurso recurso pais = 
                    pais {
                        recursosNaturales = filter(\ recu ->  (/= recurso) recu) $ recursosNaturales pais,
                        deuda = deuda pais - 2000000
                    }

poblacionActiva :: Pais -> Int
poblacionActiva pais =  poblacionPrivada pais + poblacionPublica pais

montoBlindaje :: Pais -> Float
montoBlindaje = (/ 2) . pbi

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionActiva pais)

blindaje :: Receta
blindaje pais = reducirPoblacionPublica 500 . prestar (montoBlindaje pais) $ pais

-- 3

receta :: RecursoNatural -> Receta
receta recurso pais = (otorgarRecurso recurso) . (prestar 200000000) $ pais 

--receta namibia

-- 4 

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter(\pais -> (elem "petroleo". recursosNaturales) pais)

totalDeuda :: [Pais] -> Float
totalDeuda = foldl(\total pais -> total + deuda pais)  0

--5

estaOrdenada [] _ = True
estaOrdenada [receta] _ = True

estaOrdenada (receta1: receta2: recetas) pais 
    | (pbi.receta1) pais < (pbi.receta2) pais = estaOrdenada recetas pais
    | otherwise = False


-- 6 

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos


-- Con 4.a entro en loop infinito porque no termina de recorrer nunca la lista de recursos naturales del pais
-- Con 4.b funciona ok porque haskell utiliza lazy evaluation y solo se consultan los valores de las deudas

