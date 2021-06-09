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
}

data Viaje = Viaje {
    fecha :: (Number, Number, Number)
    ,cliente :: Cliente
    ,costo :: Number
}

data Cliente = Cliente {
    nombreCliente :: String
    ,dondeVive :: String
}

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

--Punto 3a)
lucas = Cliente "Lucas" "Victoria"
viajeLucas = [(Viaje (20,04,2017) lucas 150)]

--Punto 3b)
daniel = Chofer "Daniel" 23500 viajeLucas (viajeSegunDondeViveCliente "Olivos")

--Punto 3c)
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
{-
(4 puntos) Realizar un viaje: dado un viaje y una lista de choferes, se pide que
filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: 
el viaje no se puede realizar.
-}

