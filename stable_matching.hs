import Data.List



type Person = Int
type Women  = [Person]
type Men    = [Person]
type Pref   = [Person]
type Matching = (Person, Person)

n :: Int
n = 3

-- should introduce -1 as dummy man

male_opt_matching :: Pref -> Pref -> [Matching]
male_opt_matching = undefined



allperms = permutations [1..3]


initMatching :: [Matching]
initMatching = map matchDummy [1..n]
  where matchDummy w = (-1, w)

-- The ordered list encodes the perference list
-- from favorite to unwanted
-- The function returns the more preferred person in the pref
morePref :: Pref -> Person -> Person -> Person
morePref [] _ _ = -1 -- error
morePref (h:tl) a b 
  | h == a = a
  | h == b = b
  | otherwise = morePref tl a b

matchedWomen :: [Matching] -> [Person]
matchedWomen [] = []
matchedWomen (h:tl) = (woman h) : (matchedWomen tl)
  where woman :: Matching -> Person
        woman (_ , b) = b


-- a man should look at the current matching and tried to propose
-- it should be feasible




feasibleFavorite ::  Pref -> [Matching] -> [Pref] -> Person
feasibleFavorite = undefined

propose :: Person -> Pref -> [Matching] -> [Matching]
propose = undefined




















