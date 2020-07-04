module Main where

data Tag = Fighter | Tank | Marksman | Assassin | Mage | Support
         deriving (Show, Eq)
                                       
data Champion =
     Champion {name :: String,
              attack :: Int,
              defense :: Int,
              magic :: Int,
              difficulty :: Int,
              tags :: []Tag}
     deriving (Show, Eq)

aatrox :: Champion
aatrox = Champion {name = "Aatrox",
                       attack = 8,
                       defense = 4,
                       magic = 3,
                       difficulty = 4,
                       tags = [Fighter, Tank]
                       }
darius :: Champion
darius = Champion {name = "Darius",
                   attack = 9,
                   defense = 5,
                   magic = 1,
                   difficulty = 2,
                   tags = [Fighter, Tank]
                  }


fiora :: Champion
fiora = Champion {name = "Fiora",
                  attack = 10,
                  defense = 4,
                  magic = 2,
                  difficulty = 3,
                  tags = [Fighter, Assassin]
                 }

gnar :: Champion
gnar = Champion {name = "Gnar",
                 attack = 6,
                 defense = 5,
                 magic = 5,
                 difficulty = 8,
                 tags = [Fighter, Tank]
                 }

irelia :: Champion
irelia = Champion {name = "Irelia",
                  attack = 7,
                  defense = 4,
                  magic = 5,
                  difficulty = 5,
                  tags = [Fighter, Assassin]
                  }

karma :: Champion
karma = Champion {name = "Karma",
                  attack = 1,
                  defense = 7,
                  magic = 8,
                  difficulty = 5,
                  tags = [Mage, Support]
                  }

maokai :: Champion
maokai = Champion {name = "Maokai",
                   attack = 3,
                   defense = 8,
                   magic = 6,
                   difficulty = 3,
                   tags = [Tank, Mage]
                  }

neeko :: Champion
neeko = Champion {name = "Neeko",
                  attack = 1,
                  defense = 1,
                  magic = 9,
                  difficulty = 5,
                  tags = [Mage, Support]
                  }

sylas :: Champion
sylas = Champion {name = "Sylas",
                  attack = 3,
                  defense = 4,
                  magic = 8,
                  difficulty = 5,
                  tags = [Mage, Assassin]
                  }

vayne :: Champion 
vayne = Champion {name = "Vayne",
                  attack = 10,
                  defense = 1,
                  magic = 1,
                  difficulty = 8,
                  tags = [Marksman, Assassin]
                  }

champions :: [Champion]
champions = [aatrox,
             darius,
             fiora,
             gnar,
             irelia,
             karma,
             maokai,
             neeko,
             sylas,
             vayne
            ]

data Preference =
    Preference {summonerName :: String,
                minAttack :: Int,
                minDefense :: Int,
                minMagic :: Int,
                maxDifficulty :: Int,
                prefTags :: []Tag}
     deriving (Show, Eq)

alicePref :: Preference
alicePref = Preference {  summonerName = "Alice",
                          minAttack = 2,
                          minDefense = 1,
                          minMagic = 4,
                          maxDifficulty = 3,
                          prefTags = [Mage]
                         }

bobPref :: Preference
bobPref = Preference {  summonerName = "Bob",
                        minAttack = 2,
                        minDefense = 5,
                        minMagic = 2,
                        maxDifficulty = 1,
                        prefTags = [Tank, Support]}

preferences :: [Preference]
preferences = [ alicePref,
                bobPref
              ]


highAttackChamps :: [Champion] -> [Champion]
highAttackChamps champList = filter (\ champion -> attack champion >5) champList

matchChampionAttack :: Preference -> Champion -> Bool
matchChampionAttack pref champ = (minAttack pref) <= (attack champ) 

matchChampionDefense :: Preference -> Champion -> Bool
matchChampionDefense pref champ = (minDefense pref) <= (defense champ) 

matchChampionMagic :: Preference -> Champion -> Bool
matchChampionMagic pref champ = (minMagic pref) <= (magic champ) 

matchChampionDifficulty :: Preference -> Champion -> Bool
matchChampionDifficulty pref champ = (maxDifficulty pref) >= (difficulty champ) 

matchChampionTag :: Preference -> Champion -> Bool
matchChampionTag pref champ = (elem tag (prefTags pref) && elem tag (tags champ)) =:= True where tag free

matchChampionAny :: Preference -> Champion -> Bool
matchChampionAny pref champ =  matchChampionAttack pref champ ||
                               matchChampionDefense pref champ ||
                               matchChampionMagic pref champ ||
                               matchChampionDifficulty pref champ ||
                               matchChampionTag pref champ


matchChampionAll :: Preference -> Champion -> Bool
matchChampionAll pref champ =  matchChampionAttack pref champ &&
                                 matchChampionDefense pref champ &&
                                 matchChampionMagic pref champ &&
                                 matchChampionDifficulty pref champ &&
                                 matchChampionTag pref champ

recommendWeak :: Preference -> [Champion] -> [String]
recommendWeak preference champList = map (\ champion -> name champion) (filter (\ champion -> matchChampionAny preference champion ) champList)

recommendStrong :: Preference -> [Champion] -> [String]
recommendStrong preference champList = map (\champion -> name champion) (filter (\ champion -> matchChampionAll preference champion ) champList)

example1 :: [Champion]
example1 = highAttackChamps champions

example2 :: Bool
example2 = matchChampionAttack alicePref aatrox

example3 :: Bool
example3 = matchChampionDifficulty bobPref vayne 

example4 :: Bool
example4 =  matchChampionTag alicePref karma

example5 :: Bool
example5 =  matchChampionAny alicePref neeko

example6 :: Bool
example6 =  matchChampionAll bobPref gnar

example7 :: [String]
example7 =  recommendWeak alicePref champions

example8 :: [String]
example8 =  recommendWeak bobPref champions

example9 :: [String]
example9 =  recommendStrong alicePref champions

example10 :: [String]
example10 =  recommendStrong bobPref champions

main :: IO ()
main = putStrLn ("Example 1, champions with more than 5 attack:" ++ "\n" ++
                 (show example1) ++ "\n" ++
                 "Example 2, matching Aatrox with Alice's preferences based on attack:" ++ "\n" ++
                 (show example2) ++ "\n" ++
                 "Example 3, matching Vayne with Bob's preferences based on difficulty:" ++ "\n" ++
                 (show example3) ++ "\n" ++
                 "Example 4, matching Karma with Alice's preferences based on tag(s):" ++ "\n" ++
                 (show example4) ++ "\n" ++
                 "Example 5, matching Neeko with Alice's preferences based on any matching criteria:" ++ "\n" ++
                 (show example5) ++ "\n" ++
                 "Example 6, matching Gnar with Bob's preferences based on all matching criteria:" ++ "\n" ++
                 (show example6) ++ "\n" ++
                 "Example 7, recommendation based on a weak criteria for Alice:" ++ "\n" ++
                 (show example7) ++ "\n" ++
                 "Example 8, recommendation based on a weak criteria for Bob:" ++ "\n" ++
                 (show example8) ++ "\n" ++
                 "Example 9, recommendation based on a strong criteria for Alice:" ++ "\n" ++
                 (show example9) ++ "\n" ++
                 "Example 10, recommendation based on a strong criteria for Bob:" ++ "\n" ++
                 (show example10))
