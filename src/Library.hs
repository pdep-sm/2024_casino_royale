module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Palo = String
type Carta = (Number, Palo)
numero = fst
palo = snd
type Mano = [Carta]
data Jugador = Jugador {
                nombre :: String,
                mano :: Mano,
                bebidaPreferida :: String
            } deriving Show

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

jamesBond = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre = Jugador "Le Chiffre" fullDeJokers "Gin"
felix = Jugador "Felix Leiter" piernaDeNueves "Whisky"

{- 1.a 
mayorSegun/3, que dada una función y dos valores nos devuelve aquel valor que hace mayor a la función (en caso de igualdad, cualquiera de los dos).
-}
mayorSegun func v1 v2
    | func v1 > func v2 = v1    
    | otherwise         = v2

{- 1.b
maximoSegun/2, que dada una función y una lista de valores nos devuelve aquel valor de la lista que hace máximo a la función.

-- Version anterior recursiva
maximoSegun _ [e] = e
maximoSegun f (e1:e2:es) = maximoSegun f (mayorSegun f e1 e2 : es)
-}
maximoSegun f lista = foldl1 (mayorSegun f) lista
--maximoSegun' f (e:es) = foldl (mayorSegun f) e es --No tiene sentido, es igual que la anterior

{- 1.c
sinRepetidos/1, que dada una lista devuelve la misma sin elementos repetidos. Los elementos tienen que aparecer en el mismo orden que en la lista original (la primera ocurrencia de la lista de izquierda a derecha).
> sinRepetidos [1,2,3,4,1]
[1,2,3,4]
-}
sinRepetidos [] = []
sinRepetidos (x:xs) = x : (sinRepetidos . filter (x/=)) xs

{- 2.a
esoNoSeVale/1, que se cumple para una carta inválida, ya sea por número o por palo (ver arriba cómo tiene que ser una carta).
-}
numerosValidos = [1..13]
esoNoSeVale = not.esoSeVale
esoSeVale (numero, palo) = elem numero numerosValidos && elem palo palos

{- 2.b
manoMalArmada/1, que dado un jugador, nos indica si tiene una mano mal armada. Esto es cuando sus cartas no son exactamente 5, o alguna carta es inválida.
-}
manoMalArmada jugador = ((/=5) . length . mano) jugador || (any esoNoSeVale . mano) jugador
-- Volemos en 15 manoMalArmada' jugador = ((/=5) . length . mano) jugador || (any esoNoSeVale . mano) jugador
--manoBienArmada jugador =((==5) . length . mano) jugador && (all esoSeVale . mano) jugador

{- 3
Dada una lista de cartas, hacer las funciones que verifican si las mismas forman un juego dado, según las siguientes definiciones:
    par --> tiene un número que se repite 2 veces
    pierna --> tiene un número que se repite 3 veces
    color -->  todas sus cartas son del mismo palo
    fullHouse --> es, a la vez, par y pierna
    poker --> tiene un número que se repite 4 veces
    otro --> se cumple para cualquier conjunto de cartas
-}
ocurrenciasDe x lista = (length . filter (== x)) lista

ocurrenciasDeNumeros cantidad cartas = (any ((==cantidad). flip ocurrenciasDe (map numero cartas) ) . map numero) cartas

par cartas    = ocurrenciasDeNumeros 2 cartas

pierna cartas = ocurrenciasDeNumeros 3 cartas

color cartas  = any ((==5).flip ocurrenciasDe (map palo cartas)) palos
color' (carta:cartas) = all (==palo carta) (map palo cartas)

fullHouse cartas = par cartas && pierna cartas

poker cartas = ocurrenciasDeNumeros 4 cartas

otro:: Mano -> Bool -- restricción para Mano
otro _ = True

{- 4 
alguienSeCarteo/1, dada una lista de jugadores. Sabemos que alguien se carteó cuando hay alguna carta que se repite, ya sea en un mismo jugador o en distintos.
-}
concatenar = foldl (++) []
alguienSeCarteo jugadores = sinRepetidos totalDeCartas /= totalDeCartas
    where totalDeCartas = (concatenar . map mano) jugadores 
    -- Definición Local




















