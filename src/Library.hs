module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Punto 1
data Chofer = Chofer {
    nombre :: String
    ,kilometraje :: Number
    ,viajes :: [Viaje]
    ,condicion :: Condicion
} deriving (Show)

data Viaje = Viaje {
    fecha :: (Number, Number, Number)
    ,cliente :: Cliente
    ,costo :: Number
}deriving (Show)

data Cliente = Cliente {
    nombreCliente :: String
    ,dondeVive :: String
} deriving (Show)

--Punto 2 CONDICIONES

type Condicion = Viaje -> Bool

cualquierViaje :: Condicion
cualquierViaje _ = True

viajeAMasDe200Pesos :: Condicion
viajeAMasDe200Pesos = (>200) . costo

viajeSegunNombreCliente :: Number -> Condicion
viajeSegunNombreCliente cantLetras = (> cantLetras) . length . nombreCliente . cliente 

type Zona = String

viajeSegunDondeViveCliente :: Zona -> Condicion
viajeSegunDondeViveCliente zona = not . (== zona) . dondeVive . cliente

--Punto 3a) --ACORDARSE DE DEFINIR DATAS DE EJEMPLO CON EL TIPO
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

viajeLucas :: Viaje
viajeLucas = (Viaje (20,04,2017) lucas 150)

--Punto 3b)
daniel :: Chofer
daniel = Chofer "Daniel" 23500 [viajeLucas] (viajeSegunDondeViveCliente "Olivos")

--Punto 3c)

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

--Punto 4

puedeTomarViaje :: Chofer -> Viaje -> Bool
puedeTomarViaje chofer = condicion chofer

--Punto 5
liquidacionChofer :: Chofer -> Number
liquidacionChofer = sum . listaCostos . viajes 

listaCostos :: [Viaje] -> [Number]
listaCostos viajes = map costo viajes

--Punto 6a
--Ejemplo lista de choferes y explico a Juana para probar
listaChoferes :: [Chofer]
listaChoferes = [daniel, juana] 

juana :: Chofer 
juana = Chofer "Juana" 12222 [viajeLucas, viajeDani] cualquierViaje

viajeDani :: Viaje
viajeDani = Viaje (20,04,2017) daniela 130

daniela :: Cliente
daniela = Cliente "Daniela" "Palermo"
--

realizarViaje :: Viaje -> [Chofer] -> [Chofer]
realizarViaje viaje = filter (flip puedeTomarViaje viaje)

--Punto 6b
quienTieneMenosViajesTiene :: [Chofer] -> Chofer
quienTieneMenosViajesTiene choferes = foldl1 menosViajes choferes

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes chofer1 chofer2 | chofer2TieneMenosViajes chofer2 chofer1 = chofer2
                            | otherwise = chofer1

chofer2TieneMenosViajes :: Chofer -> Chofer -> Bool
chofer2TieneMenosViajes chofer2 = ( > length (viajes chofer2)) . length . viajes 

--Punto 6c
efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer { viajes = viajes chofer ++ [viaje]}

--Punto 7a)

nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repetirViaje viajeDeNito) (viajeSegunNombreCliente 3)

viajeDeNito :: Viaje
viajeDeNito = Viaje (11,03,2017) lucas 50

repetirViaje :: Viaje -> [Viaje]  --LISTA INFINITA CON RECURSIVIDAD
repetirViaje viaje = viaje : repetirViaje viaje

--Punto 7b)
{- ¿Puede calcular la liquidación de Nito? Justifique.
No, porque para poder calcular la liquidacion de Nito necesito conocer el costo de todos los viajes que realizo. 
Como Nito tiene una lista infinita de viajes, Haskell se va a colgar y no va a poder obtener la liquidacion porque no 
llega a conocer todos los viajes que realizo Nito
-}

--Punto 7c)
{-
¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 
Si. 
-}

--Punto 8
