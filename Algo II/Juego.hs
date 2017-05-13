data Juego =   IniciarJuego Mapa
             | AgregarJugador Jugador Juego
             | AgregarPokemon PokemonSalvaje Juego
             | MoverJugador Jugador Posicion Juego
             | ConectarJugador Jugador Posicion Juego 

--Observadores basicos

mapa                :: Juego -> Mapa
jugadores           :: Juego -> [Jugador]
sanciones           :: Jugador -> Juego -> Int
conectado           :: Jugador -> Juego -> Bool
posicion            :: Jugador -> Juego -> Posicion
pokemonesCapturados :: Jugador -> Juego -> [[Pokemon]]
pokemonesSalvajes   :: Juego -> [PokemonSalvaje]
movimientosExternos :: PokemonSalvaje -> Juego -> Int

--Otras Operaciones

jugadoresValidos        :: [Jugador] -> Juego -> [Jugador]
movimientoIncremental   :: Posicion -> Posicion -> Bool
loPuedenAtrapar         :: PokemonSalvaje -> [Jugador] -> Juego -> [Jugador]
puedoAtrapar            :: Jugador -> [PokemonSalvaje] -> Juego -> Bool
puedeAtraparA           :: Jugador -> [PokemonSalvaje] -> Juego
porAtraparse            :: [PokemonSalvaje] -> Posicion -> Juego -> [PokemonSalvaje]
losAtrapados            :: [Jugador] -> Juego -> [[Pokemon]] 
tiposDePokes            :: [PokemonSalvaje] -> [[Pokemon]]
todosLosPokes           :: Juego -> [[Pokemon]]
rareza                  :: Pokemon -> Juego -> Int

--Axiomas

mapa IniciarJuego m = m
mapa AgregarJugador j jg      = mapa jg
mapa AgregarPokemon J jg      = mapa jg
mapa MoverJugador j p jg      = mapa jg
mapa ConectarJugador j p jg   = mapa jg

jugadores IniciarJuego m            = []
jugadores AgregarJugador j jg       = j: (jugadores jg)
jugadores AgregarPokemon p jg       = jugadores jg
jugadores MoverJugador j p jg       = jugadores jg
jugadores ConectarJugador j p jg    = jugadores jg

sanciones()