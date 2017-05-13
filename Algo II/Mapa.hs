data Mapa = CrearMapa | AgregarCamino Posicion Posicion Mapa

--Observadores básicos

posiciones :: Mapa -> [Posicion]

hayCamino :: Posicion -> Posicion -> Mapa -> Bool
 
--Otras Operaciones

posicionValida :: Posicion -> Mapa -> Bool

--Axiomas

posiciones CrearMapa = []
posiciones (AgregarCamino p1 p2 m) = p1:p2:(Posiciones m)

hayCamino _ _ CrearMapa = False
hayCamino p1 p2 (AgregarCamino p3 p4 m) = | ([p1, p2] `elem` [[p3, p4], [p4, p3]])              -> True              -> True
                                          | (hayCamino p1 p3 m) and (hayCamino p2 p4 m) -> True
                                          | (hayCamino p1 p4 m) and (hayCamino p2 p3 m) -> True
                                          | otherwise                                   -> False

posicionValida i m = i `elem` (posiciones m)
