module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Palo = String
type Carta = (Number, Palo)
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
-}
maximoSegun _ [e] = e
maximoSegun f (e1:e2:es) = maximoSegun f (mayorSegun f e1 e2 : es)

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




