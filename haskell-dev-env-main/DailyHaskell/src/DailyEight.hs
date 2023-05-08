--Lucy Kien
--Daily Eight
--Programming Languages 

module DailyEight where

    --Event data type, which has these fields: name, day, month, year, xlocation, ylocation. 
    data Event = Event
                    { name :: String
                    , day :: Int
                    , month :: Int
                    , year :: Int
                    , xlocation :: Double
                    , ylocation :: Double
                    } deriving (Show, Eq)

    -- inYear, which takes a number called, year, and a list of event structures
    --produces a new list of events
    --Each element in the new list should be an event structure which occurred during year.
    inYear :: Int -> [Event] -> [Event]
    inYear y events = filter (\e -> year e == y) events

    --inDayRange, which takes two days (a start day and an end day) and a list of event structures
    --produces a new list. 
    --Each element in the new list should be the name of all events which occurred between the two days (including start day and end day) but in any month or year.
    inDayRange :: Int -> Int -> [Event] -> [String]
    inDayRange startDay endDay events = 
        [ name e | e <- events, day e >= startDay, day e <= endDay ]

    --inArea, which takes a name, a lower x location, an upper x location, a lower y location, an upper y location and a list of event structures
    --produces a new list. 
    --Each element in the new list should be the event structures which match the name and  occurred in the spatial region specified.
    inArea :: String -> Double -> Double -> Double -> Double -> [Event] -> [Event]
    inArea n x1 x2 y1 y2 events =
        filter (\e -> name e == n && xlocation e >= x1 && xlocation e <= x2 && ylocation e >= y1 && ylocation e <= y2) events