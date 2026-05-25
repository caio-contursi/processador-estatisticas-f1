import Data.List (sortBy)

data RaceData = RaceData {
    driverName :: String,
    lapTimes   :: [Double],
    penalties  :: Double,
    position   :: Int
} deriving (Show, Eq)

data DriverStats = DriverStats {
    name         :: String,
    points       :: Int,
    averageSpeed :: Double
} deriving (Show, Eq)

calculatePoints :: Int -> Int
calculatePoints 1 = 25
calculatePoints 2 = 18
calculatePoints 3 = 15
calculatePoints 4 = 12
calculatePoints 5 = 10
calculatePoints 6 = 8
calculatePoints 7 = 6
calculatePoints 8 = 4
calculatePoints 9 = 2
calculatePoints 10 = 1
calculatePoints _ = 0

calculateTotalTime :: RaceData -> Double
calculateTotalTime race = sum (lapTimes race) + penalties race

calculateAverageSpeed :: Double -> RaceData -> Double
calculateAverageSpeed trackLength race =
    let totalDistance = trackLength * fromIntegral (length (lapTimes race))
        totalTime = calculateTotalTime race
    in if totalTime > 0 then totalDistance / totalTime else 0

processRaceData :: Double -> RaceData -> DriverStats
processRaceData trackLength race = DriverStats {
    name         = driverName race,
    points       = calculatePoints (position race),
    averageSpeed = calculateAverageSpeed trackLength race
}

sortStandings :: [DriverStats] -> [DriverStats]
sortStandings = sortBy compareStats
  where
    compareStats a b = 
        case compare (points b) (points a) of
            EQ -> compare (averageSpeed b) (averageSpeed a)
            other -> other

generateChampionshipStandings :: Double -> [RaceData] -> [DriverStats]
generateChampionshipStandings trackLength raceDataList =
    sortStandings (map (processRaceData trackLength) raceDataList)

main :: IO ()
main = do
    let trackLength = 5000.0 
    
    let rawData = [
            RaceData "Piloto A" [85.5, 84.2, 84.0] 0.0 1,
            RaceData "Piloto B" [86.1, 85.0, 85.5] 5.0 3,
            RaceData "Piloto C" [85.0, 84.5, 84.1] 0.0 2,
            RaceData "Piloto D" [88.0, 87.5, 87.1] 10.0 4
          ]

    let finalStandings = generateChampionshipStandings trackLength rawData

    putStrLn "--- CLASSIFICAÇÃO DO CAMPEONATO ---"
    mapM_ (\stats -> putStrLn $ 
        name stats ++ 
        " | Pontos: " ++ show (points stats) ++ 
        " | Vel. Média: " ++ show (round (averageSpeed stats * 3.6)) ++ " km/h") finalStandings
