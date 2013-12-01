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

--  find out whether a woman would want to deviate 
wouldCheat :: [Pref] -> Person -> Matching -> Bool
wouldCheat prefs  person (m, w) = morePref (prefs !! w) m person == person


-- some dude wants to see which girls are in his league
inTheLeague :: Person -> [Pref] -> [Matching] -> [Person]
inTheLeague man womenPrefs matchings = map woman list
  where list = filter (wouldCheat womenPrefs man) matchings
        woman :: Matching -> Person
        woman (_, b) = b



matchedMen :: [Matching] -> [Person]
matchedMen [] = []
matchedMen (h:tl) = (man h) : (matchedMen tl)
  where man :: Matching -> Person
        man (a , _) = a

unmatchedMen :: [Matching] -> [Person]
unmatchedMen l = [1..n] \\ ( matchedMen l )




-- a man should look at the current matching and tried to propose
-- it should be feasible
feasibleFavorite ::  Person -> [Pref] -> [Pref] -> [Matching] -> Person
feasibleFavorite man menPrefs womenPrefs matchings = 
  favorite (menPrefs !! man) list
  where list = inTheLeague man womenPrefs matchings
        favorite :: Pref -> [Person] -> Person
        favorite (h:tl) list = if (elem h list)
                               then h
                               else favorite tl list



propose :: Person -> [Pref] -> [Pref] -> [Matching] -> [Matching]
propose man menPrefs womenPrefs matchings = newMatching
  where theOne = feasibleFavorite man menPrefs womenPrefs matchings
        newMatching = updateMatching matchings
        updateMatching :: [Matching] -> [Matching]
        updateMatching ((m,w):tl) = if (w == theOne)
                                    then ((man, w):tl)
                                    else (m,w):(updateMatching tl)
        updateMatching [] = []


-- this is the function that keeps 
match :: [Matching] -> [Matching]

















