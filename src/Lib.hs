module Lib where
import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

--1)

putter :: Palo
putter habilidad = UnTiro { velocidad = 10 , precision = ((*2) . precisionJugador) habilidad , altura = 0 } 

madera :: Palo
madera habilidad = UnTiro { velocidad = 100 , precision = precisionJugador habilidad `div` 2 , altura = 5 } 

hierro :: Int -> Palo
hierro n habilidad = UnTiro { velocidad = ((*n) . fuerzaJugador) habilidad , precision = precisionJugador habilidad `div` n , altura = max 0 (n-3) } 

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

--2)

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador) 

--3)

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  actualizaTiro :: Tiro -> Tiro
} 

tiroDespuesDePasarPorObstaculo :: Obstaculo -> Tiro -> Tiro
tiroDespuesDePasarPorObstaculo obstaculo tiro
                  | puedeSuperar obstaculo tiro = actualizaTiro obstaculo tiro
                  | otherwise = detenerTiro tiro

detenerTiro :: Tiro -> Tiro
detenerTiro tiro = tiro { velocidad = 0 , precision = 0 , altura = 0 } 

tiroSuperaTunel :: Tiro -> Bool
tiroSuperaTunel tiro = precision tiro > 90 && vaAlRasSuelo tiro

vaAlRasSuelo :: Tiro -> Bool
vaAlRasSuelo = (==0) . altura

actualizarTiroTunel :: Tiro -> Tiro
actualizarTiroTunel tiro = tiro { velocidad = ((*2). velocidad) tiro , precision = 100 , altura = 0 }

tiroSuperaLaguna :: Tiro -> Bool
tiroSuperaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro) 

actualizarTiroLaguna :: Int -> Tiro -> Tiro
actualizarTiroLaguna largoLaguna tiro = tiro { altura = (altura tiro) `div` largoLaguna } 

tiroSuperaHoyo :: Tiro -> Bool
tiroSuperaHoyo tiro = between 5 20 (velocidad tiro) && (precision tiro) > 95 

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo tiroSuperaTunel actualizarTiroTunel

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo tiroSuperaLaguna (actualizarTiroLaguna largo)

hoyo :: Obstaculo
hoyo = UnObstaculo tiroSuperaHoyo detenerTiro

--4)a)

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo  = filter (superaObstaculo jugador obstaculo) palos

superaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
superaObstaculo jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

--4)b)--Forma recursiva
-- cantidadObstaculosConsecutivosRecursivo :: Tiro -> [Obstaculo] -> Int
-- cantidadObstaculosConsecutivosRecursivo tiro [] = 0
-- cantidadObstaculosConsecutivosRecursivo tiro (cabeza: cola)
--        | puedeSuperar cabeza tiro = 1 + cantidadObstaculosConsecutivosRecursivo (actualizaTiro cabeza tiro) cola
--        | otherwise = 0

altoTiro = UnTiro 10 95 0 

cantidadObstaculosConsecutivos :: Tiro -> [Obstaculo] -> Int
cantidadObstaculosConsecutivos tiro = length . takeWhile (supera tiro) 

supera :: Tiro -> Obstaculo -> Bool
supera tiro obstaculo = puedeSuperar obstaculo tiro

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos  =  maximoSegun (flip cantidadObstaculosConsecutivos obstaculos . golpe jugador ) palos

--5)

perdedores :: [(Jugador, Puntos)] -> [String]
perdedores lista  = (map (padre.fst) . filter (not . gano lista)) lista

gano ::  [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano lista tupla =  snd tupla == (calcularPuntajeGanador lista)

calcularPuntajeGanador :: [(Jugador, Puntos)] -> Int
calcularPuntajeGanador  =  maximum . map snd 