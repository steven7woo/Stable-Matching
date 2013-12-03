import Data.List



type Person = Integer
type Women  = [Person]
type Men    = [Person]
type Pref   = [Person]
type Matching = (Person, Person)

--n= 3                            
l = [1..3]
list = [l, [3,2,1],l]
pref = (list, list)
matching = match pref
subPrefs = map (getSubPrefs pref) matching



-- should introduce -1 as dummy man

male_opt_matching :: Pref -> Pref -> [Matching]
male_opt_matching = undefined



allperms = permutations [1..3]


initMatching :: Integer -> [Matching]
initMatching n = map matchDummy [1..n]
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

-- retrive a woman's preference list  
getWomanPref :: ([Pref], [Pref]) -> Person -> Pref
getWomanPref (menPrefs, womenPrefs) woman = let
  women = sort $ head menPrefs
  index = length $ fst $ splitAt (fromIntegral woman) women
  in womenPrefs !! (index - 1)


-- retrive a man's preference list  
getManPref :: ([Pref], [Pref]) -> Person -> Pref
getManPref (menPrefs, womenPrefs) man = let
  men = sort $ head womenPrefs
  index = length $ fst $ splitAt (fromIntegral man) men 
  in menPrefs !! (index - 1)

--  find out whether a woman would want to deviate if the person proposes
wouldCheat :: ([Pref], [Pref]) -> Person -> Matching -> Bool
wouldCheat prefs person (m, w) = morePref (getWomanPref prefs w) m person
                                 == person


-- some dude wants to see which girls are in his league
inTheLeague :: Person -> ([Pref], [Pref]) -> [Matching] -> [Person]
inTheLeague man prefs matchings = map woman list
  where list = filter (wouldCheat prefs man) matchings
        woman :: Matching -> Person
        woman (_, b) = b



matchedMen :: [Matching] -> [Person]
matchedMen [] = []
matchedMen (h:tl) = (man h) : (matchedMen tl)
  where man :: Matching -> Person
        man (a , _) = a

unmatchedMen :: [Matching] -> [Person]
unmatchedMen l = [1..n] \\ ( matchedMen l )
  where n = toInteger $ length l



-- a man should look at the current matching and tried to propose
-- it should be feasible
feasibleFavorite ::  Person -> [Pref] -> [Pref] -> [Matching] -> Person
feasibleFavorite man menPrefs womenPrefs matchings = 
  favorite (getManPref (menPrefs, womenPrefs) man) list
  where list = inTheLeague man (menPrefs, womenPrefs) matchings
        favorite :: Pref -> [Person] -> Person
        favorite (h:tl) list = if (elem h list)
                               then h
                               else favorite tl list
        favorite [] _ = -1


propose :: Person -> [Pref] -> [Pref] -> [Matching] -> [Matching]
propose man menPrefs womenPrefs matchings = newMatching
  where theOne = feasibleFavorite man menPrefs womenPrefs matchings
        newMatching = updateMatching matchings
        updateMatching :: [Matching] -> [Matching]
        updateMatching ((m,w):tl) = if (w == theOne)
                                    then ((man, w):tl)
                                    else (m,w):(updateMatching tl)
        updateMatching [] = []


-- this is the function that matches people
match :: ([Pref] , [Pref]) -> [Matching]
match (menPrefs, womenPrefs) = match' (initMatching m) (menPrefs, womenPrefs)
  where 
    m = toInteger $ length (head menPrefs)
    match' :: [Matching] -> ([Pref], [Pref]) -> [Matching]
    match' matchings (menPrefs, womenPrefs) =  if (null losers)
                                               then matchings -- we are done here
                                               else match' (propose (head losers) menPrefs womenPrefs matchings) $ (menPrefs, womenPrefs)
      where losers = unmatchedMen matchings


-- allPrefs :: [[Pref]]
-- allPrefs = [[a,b,c] | a<-list, b<-list, c<-list ]
--   where list = permutations [1..n]


-- allPrefComb :: [([Pref], [Pref])]
-- allPrefComb = [(a, b) | a<- allPrefs, b<-allPrefs]


-- Remove The I-Th Indexed Element From A List
removeIndex :: Int -> [a] -> [a]
removeIndex _ [] = []
removeIndex i xs = let (h,t) = splitAt (i - 1) xs in h ++ (tail t)  

-- generate sub-preference list for each match (m, w)
-- TODO needs to fix the removeIndex part


getSubPrefs :: ([Pref], [Pref]) -> Matching -> ([Pref], [Pref])
getSubPrefs (menPrefs, womenPrefs) (man, woman) = let
  menPref1 = (removeIndex (fromIntegral man) menPrefs)
  womenPref1 = (removeIndex (fromIntegral woman) womenPrefs)
  menPref2 = map (removeIndex (fromIntegral man)) menPref1
  womenPref2 = map (removeIndex (fromIntegral woman)) womenPref1
  in (menPref2, womenPref2)



-- generate new instance for reduced markets from a male opt matching
submatch ::  ([Pref], [Pref]) -> [[Matching]]
submatch (menPrefs, womenPrefs) = let
  fullmatching = match (menPrefs, womenPrefs)
  subPrefs = map (getSubPrefs (menPrefs, womenPrefs)) fullmatching
  in  map match subPrefs 




