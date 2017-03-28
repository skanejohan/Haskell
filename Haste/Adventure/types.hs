module Types where

data ItemId = EntranceDoor
            | OfficeDoor
            | Plaque 
            | LanguageShelf
            | LatinDictionary
            | TravelShelf
            | HouseHistoryBook
            | OfficeDoorKey
            | Sink
            | WaterCooker
            | Fridge
            | Cupboard
            | StorageRoomKey
            | Cup
            deriving (Ord, Eq, Show, Read)

--data DoorId = 
--            deriving (Ord, Eq, Show, Read)

data LocationId = FictionSection 
                | TravelSection
                | ArtSection
                | HistorySection
                | Office
                | Kitchen
                deriving (Ord, Eq, Show, Read)

data Flag = OfficeKeyFound
          | LatinDictionaryFound 
          | EntranceIsKnown 
          | WaterCookerIntroduced
          deriving (Ord, Eq, Show, Read)