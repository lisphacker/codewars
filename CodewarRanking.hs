module CodewarRanking where

data User = User {
      rank     :: Int
    , progress :: Int
} deriving Show

validRanks = [(-8)..(-1)] ++ [1..8]

newUser :: User
newUser = User (-8) 0

incProgress activityRank user
    | activityRank `notElem` validRanks    = error ("Invalid user - " ++ show user)
    | rank user `notElem` validRanks       = error ("Invalid user - " ++ show user)
    | progress user `notElem` [0..99]      = error ("Invalid user - " ++ show user)
    | rank user == activityRank            = threshold (makeProgress 3 user)
    | rank user == 1 && activityRank == -1 = threshold (makeProgress 1 user)
    | rank user - activityRank == 1        = threshold (makeProgress 1 user)
    | rank user - activityRank > 1         = user
    | (rank user * activityRank) < 0       = let d = activityRank - rank user - 1
                                             in threshold (makeProgress (10 * d * d) user)
    | otherwise                            = let d = activityRank - rank user
                                             in threshold (makeProgress (10 * d * d) user)
    where threshold user@(User rank progress)
              | rank >= 8  = User 8 0
              | otherwise = user
          makeProgress points user = let newScore = points + progress user
                                     in if newScore >= 100 then
                                            if rank user == -1 then
                                                makeProgress (newScore - 100) (User 1 0)
                                            else
                                                makeProgress (newScore - 100) (User (rank user + 1) 0)
                                        else
                                            User (rank user) newScore
