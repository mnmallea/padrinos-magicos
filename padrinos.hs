import Text.Show.Functions
type Habilidad = String
type Deseo = Chico -> Chico

data Chico = Chico {nombre::String, edad::Int, habilidades::[Habilidad], deseos::[Deseo]} deriving Show

timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]
chester = Chico "Chester" 10 [] [serGrosoEnNeedForSpeed, serMayor]
aj = Chico "AJ" 10 [] [aprenderHabilidades ["sacarse A", "saber manejar"], serMayor, aprenderHabilidades ["saber cocinar"]]


--Concediendo Deseos
--- Punto 1

aprenderHabilidades::[Habilidad] -> Chico -> Chico
aprenderHabilidades unasHabilidades unChico = unChico{habilidades = habilidades unChico ++ unasHabilidades}

serGrosoEnNeedForSpeed::Chico -> Chico
serGrosoEnNeedForSpeed = aprenderHabilidades habilidadesDeNeedForSpeed

habilidadesDeNeedForSpeed::[Habilidad]
habilidadesDeNeedForSpeed = map (("jugar need for speed " ++).show) [1..]

serMayor::Deseo
serMayor = modificarEdad (\ _ -> 18)

--- Punto 2
type PadrinoMagico = Chico -> Chico

modificarEdad::(Int -> Int) -> Chico -> Chico
modificarEdad modificador unChico = unChico{edad = modificador.edad $ unChico}

wanda::PadrinoMagico
wanda unChico = madurar.primerDeseo unChico $ unChico

madurar::Chico -> Chico
madurar = modificarEdad (+1)

primerDeseo::Chico -> Deseo
primerDeseo = head.deseos

cosmo::PadrinoMagico
cosmo = modificarEdad (flip div 2)

muffinMagico::PadrinoMagico
muffinMagico unChico = foldl (flip ($)) unChico. deseos $ unChico 

--En busqueda de pareja
---Punto 1

tieneHabilidad::Habilidad -> Chico -> Bool
tieneHabilidad unaHabilidad = elem unaHabilidad.habilidades

esSuperMaduro::Chico -> Bool
esSuperMaduro unChico = tieneHabilidad "saber manejar" unChico && esMayorDeEdad unChico

esMayorDeEdad:: Chico -> Bool
esMayorDeEdad = (>=18).edad

---Punto 2
type Condicion = Chico -> Bool
data Chica = Chica {nombreChica::String, condicion::Condicion}

trixie = Chica "Trixie Tang" noEsTimmy
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego") 

quienConquistaA::Chica -> [Chico] -> Chico
quienConquistaA unaChica unosPretendientes
 |algunoPuedeConquistarla unaChica unosPretendientes = elQuePuedeConquistarla unaChica unosPretendientes
 |otherwise = last unosPretendientes

elQuePuedeConquistarla::Chica -> [Chico] -> Chico
elQuePuedeConquistarla unaChica = head.filter (puedeSerConquistada unaChica)

puedeSerConquistada:: Chica -> Chico -> Bool
puedeSerConquistada unaChica unChico = (condicion unaChica) unChico

algunoPuedeConquistarla::Chica -> [Chico] -> Bool
algunoPuedeConquistarla unaChica = any (puedeSerConquistada unaChica)

noEsTimmy::Condicion
noEsTimmy = (/= "Timmy").nombre

nuevaChica = Chica "vicky vicky" (tieneHabilidad "saber cocinar")

--Da Rules

habilidadesProhibidas::[Habilidad]
habilidadesProhibidas = ["enamorar", "matar", "dominar al mundo"]

esHabilidadProhibida::Habilidad -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas

algunaEsHabilidadProhibida::[Habilidad] -> Bool
algunaEsHabilidadProhibida = any esHabilidadProhibida

esDeseoProhibidoPara::Chico -> Deseo -> Bool
esDeseoProhibidoPara unChico unDeseo = algunaEsHabilidadProhibida.primerasHabilidades 5.unDeseo $ unChico

primerasHabilidades::Int -> Chico -> [Habilidad]
primerasHabilidades unaCantidad = take unaCantidad.habilidades

tieneDeseoProhibido::Chico -> Bool
tieneDeseoProhibido unChico = any (esDeseoProhibidoPara unChico).deseos $ unChico

infractoresDeDaRules::[Chico] -> [String]
infractoresDeDaRules = map nombre.filter tieneDeseoProhibido