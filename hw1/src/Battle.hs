module Battle where

exampleMonsters     = [rabbit, wolf, bear]
examplePlayerWins   = gloriousBattle (Player 100 100 100) exampleMonsters
examplePlayerDies   = gloriousBattle (Player 1 1 1) exampleMonsters

data Monster = Monster { monsterName :: String, monsterHealth :: Int, monsterAttack :: Int, equipments :: [Equipment] } deriving (Show, Eq)

rabbit = Monster {
    monsterName     = "rabbit",
    monsterHealth   = 2,
    monsterAttack   = 1,
    equipments      = [ Aspirine 1 ]
}

wolf = Monster {
    monsterName     = "wolf",
    monsterHealth   = 10,
    monsterAttack   = 3,
    equipments      = [ Weapon 10 ]
}


bear = Monster {
    monsterName     = "bear",
    monsterHealth   = 20,
    monsterAttack   = 10,
    equipments      = [ Shield 30 ]
}

data Player = Player { health :: Int, attack :: Int, defense :: Int } deriving (Show, Eq)

data Equipment = Weapon { attackPower :: Int } | Shield { defensePower :: Int } | HealPot { healthRaise :: Int } deriving (Show, Eq)

data Result = PlayerWins { player :: Player } | MonsterWins { monster :: Monster } deriving (Show, Eq)

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle p []     = PlayerWins { player = p }
gloriousBattle p (m:ms) = case (pvp True p m) of
                            Left m -> MonsterWins { monster = m }
                            Right newP -> gloriousBattle newP ms

pvp :: Bool -> Player -> Monster -> Either Monster Player
pvp True p m
    | attack p >= monsterHealth m = Right $ updateEquipment p (equipments m)
    | otherwise                   = pvp False p (m { monsterHealth = monsterHealth m - attack p }) 
pvp False p m
    | monsterAttack m < defense p = pvp True p m
    | monsterAttack m >= health p = Left m
    | otherwise                   = pvp True (p { health = health p - monsterAttack m }) m

updateEquipment :: Player -> [Equipment] -> Player
updateEquipment p []     = p
updateEquipment p (x:xs) = updateEquipment (improve p x) xs

improve :: Player -> Equipment -> Player
improve p (Weapon ap)   = p { attack = max ap (attack p) }
improve p (Shield dp)   = p { defense = max dp (defense p) }
improve p (HealPot hr) = p { health = hr + health p }
