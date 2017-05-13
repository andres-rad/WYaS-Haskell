type Pokemon = String
type Jugador = Int
type Posicion = (Int, Int)
type PokemonSalvaje = (Pokemon, Posicion)

norma :: Posicion -> Posicion -> Int
norma (x1, y1), (x2, y2) = sqrt((x1 - x2)**2 + (y1 - y2)**2)

distMayor :: Posicion -> Posicion -> Int -> Bool
distMayor  p1 p2 n = norma(p1, p2) > n

distMenorEq :: Posicion -> Posicion -> Int -> Bool
distMenorEq p1, p2, n = not (distMayor p1 p2 n)

poke :: PokemonSalvaje -> Pokemon
poke ps = fst(ps)

donde :: PokemonSalvaje -> Posicion
donde ps = snd(ps)

