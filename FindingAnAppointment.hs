module FindingAnAppointment where

import Data.List
import Data.Char

timeOff = -540
timeMax = 599

timeStrToInt ts = ((\(h, m) -> h * 60 + m + timeOff) .  mapt ((read :: String -> Int) . filter isDigit) . break (== ':')) ts
    where mapt f (x, y) = (f x, f y)

timeIntToStr t = let t2 = t - timeOff
                     (h, m) = (t2 `div` 60, t2 `mod` 60)
                     hs = if h < 10 then '0':(show h) else show h
                     ms = if m < 10 then '0':(show m) else show m
                 in hs ++ ":" ++ ms
                   
getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = let runs = findRuns (commonFreeMinutes schedules)
                                  in case (find (\(h, l) -> l >= duration) runs) of
                                       Just h  -> Just (timeIntToStr (fst h))
                                       Nothing -> Nothing
    where findRuns = map (\(x, y, z) -> (x, y)) . foldr f []
              where f x z = if null z 
                            then 
                                [(x, 1, x)] 
                            else 
                                let (s, n, e) = head z
                                in if x + n == e then
                                       (x, n + 1, e):(tail z)
                                   else
                                       (x, 1, x):z
          meetingToMinutes (start, end) = init [(timeStrToInt start)..(timeStrToInt end)]
          freeMinutes = ((\\) [0..timeMax] . concatMap meetingToMinutes)
          commonFreeMinutes = foldl1 intersect . map freeMinutes

schedules =
    [ [("09:00", "11:30"), ("13:30", "16:00"), ("16:00", "17:30"), ("17:45", "19:00")]
    , [("09:15", "12:00"), ("14:00", "16:30"), ("17:00", "17:30")]
    , [("11:30", "12:15"), ("15:00", "16:30"), ("17:45", "19:00")]
    ]

simpleSchedule = [("09:00", "09:10"), ("09:15", "09:20")]
