module DailyEightSpec where
import Test.Hspec
import DailyEight

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "In Year Function" $ do
    let event1 = Event { name = "Event 1", day = 1, month = 1, year = 2022, xlocation = 0.0, ylocation = 0.0 }
    let event2 = Event { name = "Event 2", day = 1, month = 1, year = 2022, xlocation = 1.0, ylocation = 1.0 }
    let event3 = Event { name = "Event 3", day = 1, month = 2, year = 2022, xlocation = 2.0, ylocation = 2.0 }
    let event4 = Event { name = "Event 4", day = 1, month = 2, year = 2023, xlocation = 3.0, ylocation = 3.0 }

    it "can filter events by year" $ do
      let events = [event1, event2, event3, event4]
      inYear 2022 events `shouldBe` [event1, event2, event3]
    it "returns empty list when no events occur in given year" $ do
      let events = [event1, event2, event3, event4]
      inYear 1999 events `shouldBe` []
    it "returns only events that occur in given year" $ do
      let events = [event1, event2, event3, event4]
      inYear 2023 events `shouldBe` [event4]
    it "returns events in chronological order" $ do
      let events = [event1, event2, event3, event4]
      inYear 2022 events `shouldBe` [event1, event2, event3]

  describe "In Day Range Function" $ do
    let event1 = Event { name = "Event 1", day = 1, month = 1, year = 2022, xlocation = 0.0, ylocation = 0.0 }
    let event2 = Event { name = "Event 2", day = 2, month = 1, year = 2022, xlocation = 1.0, ylocation = 1.0 }
    let event3 = Event { name = "Event 3", day = 3, month = 2, year = 2022, xlocation = 2.0, ylocation = 2.0 }
    let event4 = Event { name = "Event 4", day = 4, month = 2, year = 2023, xlocation = 3.0, ylocation = 3.0 }

    it "Days not in range should return an empty list" $ do
      let events = [event1, event2, event3, event4]
      inDayRange 5 6 events `shouldBe` []
    it "Returns all events for the day range of 1-4" $ do
      let events = [event1, event2, event3, event4]
      inDayRange 1 4 events `shouldBe` ["Event 1", "Event 2", "Event 3", "Event 4"]
    it "returns events within a two day range" $ do
      let events = [event1, event2, event3, event4]
      inDayRange 1 3 events `shouldBe` ["Event 1", "Event 2", "Event 3"]
    it "can filter events by day range 2-3" $ do
      let events = [event1, event2, event3, event4]
      inDayRange 2 3 events `shouldBe` ["Event 2", "Event 3"]

  describe "In Area Function" $ do
    let event1 = Event { name = "Event 1", day = 1, month = 1, year = 2022, xlocation = 0.0, ylocation = 0.0 }
    let event2 = Event { name = "Event 2", day = 2, month = 1, year = 2022, xlocation = 1.0, ylocation = 1.0 }
    let event3 = Event { name = "Event 3", day = 3, month = 2, year = 2022, xlocation = 2.0, ylocation = 2.0 }
    let event4 = Event { name = "Event 4", day = 4, month = 2, year = 2023, xlocation = 3.0, ylocation = 3.0 }
    let event5 = Event { name = "Event 5", day = 5, month = 3, year = 2022, xlocation = 1.5, ylocation = 1.5 }
    
    it "returns an empty list when there are no events in the given area" $ do
      let events = [event1, event2, event3, event4]
      inArea "Event 5" 0.0 0.9 0.0 0.9 events `shouldBe` []  
    it "returns only events that match the given name and are in the specified area" $ do
      let events = [event1, event2, event3, event4, event5]
      inArea "Event 2" 0.5 1.5 0.5 1.5 events `shouldBe` [event2]   
    it "returns all events that match the given name and are in the specified area" $ do
      let events = [event1, event2, event3, event4, event5]
      inArea "Event 5" 0.0 2.0 0.0 2.0 events `shouldBe` [event5]   
    it "returns events that match the given name and are on the boundary of the specified area" $ do
      let events = [event1, event2, event3, event4, event5]
      inArea "Event 5" 1.0 2.0 1.0 2.0 events `shouldBe` [event5]