module H3 where

import           TreePrinters (Tree (..))

-- Day of week

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Thursday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Day -> Int -> Day
afterDays x 0 = x
afterDays x n = afterDays (nextDay x) (n - 1)

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Int
daysToParty Friday = 0
daysToParty x = (daysToParty (nextDay x)) + 1

-- Monsters and players

exampleMonsters     = [rabbit, wolf, bear]
examplePlayerWins   = gloriousBattle (Player 100 100 100) exampleMonsters
examplePlayerDies   = gloriousBattle (Player 1 1 1) exampleMonsters

data Monster = Monster { monsterName :: String, monsterHealth :: Int, monsterAttack :: Int, equipments :: [Equipment] } deriving (Show, Eq)

rabbit = Monster {
    monsterName     = "rabbit",
    monsterHealth   = 2,
    monsterAttack   = 1,
    equipments      = [ HealPot 1 ]
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

-- Vector 

data Vector a = Vector2D a a | Vector3D a a a

vecLength :: Floating a => Vector a -> a
vecLength (Vector2D x y) = sqrt $ x ** 2 + y ** 2
vecLength (Vector3D x y z) = sqrt $ x ** 2 + y ** 2 + z ** 2

vecSum :: Num a => Vector a -> Vector a -> Vector a
vecSum (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
vecSum (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
vecSum (Vector2D x1 y1) (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z2)
vecSum (Vector3D x1 y1 z1) (Vector2D x2 y2) = vecSum (Vector2D x2 y2) (Vector3D x1 y1 z1)

vecScalar :: Num a => Vector a -> Vector a -> a
vecScalar (Vector2D x1 y1) (Vector2D x2 y2) = (x1 * x2) + (y1 * y2)
vecScalar (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
vecScalar (Vector2D x1 y1) (Vector3D x2 y2 z2) = (x1 * x2) + (y1 * y2) 
vecScalar (Vector3D x1 y1 z1) (Vector2D x2 y2) = vecScalar (Vector2D x2 y2) (Vector3D x1 y1 z1)

vecDist :: Floating a => Vector a -> Vector a -> a
vecDist (Vector2D x1 y1) (Vector2D x2 y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
vecDist (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2
vecDist (Vector2D x1 y1) (Vector3D x2 y2 z2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 
vecDist (Vector3D x1 y1 z1) (Vector2D x2 y2) = vecDist (Vector2D x2 y2) (Vector3D x1 y1 z1)

vecCrossProduct :: Num a => Vector a -> Vector a -> Vector a
vecCrossProduct (Vector2D x1 y1) (Vector2D x2 y2) = (Vector3D 0 0 (x1 * y2 - y1 * x2))
vecCrossProduct (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = (Vector3D (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2))
vecCrossProduct (Vector2D x1 y1) (Vector3D x2 y2 z2) = (Vector3D (y1*z2) (-x1*z2) (x1*y2 - y1*x2))
vecCrossProduct (Vector3D x1 y1 z1) (Vector2D x2 y2) = vecCrossProduct (Vector2D (-x2) (-y2)) (Vector3D x1 y1 z1)

-- Natural number

data Nat = Z | S Nat deriving (Show)

natSum :: Nat -> Nat -> Nat
natSum x Z = x
natSum x (S y) = S $ natSum x y

natMul :: Nat -> Nat -> Nat
natMul Z y = Z
natMul (S Z) y = y
natMul (S x) y = natSum y $ natMul x y

natSubtract :: Nat -> Nat -> Nat
natSubtract (S x) (S y) = natSubtract x y
natSubtract x Z = x
natSubtract Z _ = error "can't represent negative result"

natConstruct :: Int -> Nat
natConstruct n 
      | n < 0 = error "number must be non-negative"
      | n == 0 = Z
      | n > 0 = S $ natConstruct (n-1) 

instance Eq Nat where
  (==) Z Z = True
  (==) (S x) (S y) = x == y
  (==) _ _ = False

instance Ord Nat where 
  (<=) (S x) (S y) = x < y
  (<=) Z _ = True
  (<=) _ _ = False

-- Find tree

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

size :: Tree a -> Int
size Leaf = 0
size (Node n l r) = 1 + (size l) + (size r)

find :: (Ord a) => a -> Tree a -> Maybe a
find _ Leaf = Nothing
find x (Node n left right)
    | n == x    = Just n
    | x < n     = find x left
    | otherwise = find x right

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node n left right)
    | n == x    = Node n left right
    | x < n     = Node n (insert x left) right
    | otherwise = Node n left (insert x right)

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = toList l ++ [x] ++ toList r

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf 

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

instance Ord a => Monoid (Tree a) where
    mempty = Leaf
    mappend t1 t2 = foldr insert t1 t2

