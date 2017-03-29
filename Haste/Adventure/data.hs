module Data where

import Types
import Framework

--------------------------------------------------------------------------------------------------------------------
-----
----- Travel section
-----
--------------------------------------------------------------------------------------------------------------------

travelSection :: Location
travelSection = location "travel section" "You are in the travel and language section of the book shop. One of the shelves in here holds dictionaries, grammar guides and the like. The other holds books about locations near and far, exotic ones about distant cultures and sandy deserts, as well as less exotic ones such as Islington and Ipswich. In one corner stands a worn armchair. Many a time have you sat here, dreaming of faraway beaches and cities where the lights never go out. Through the window, you can see the street outside, currently covered in a thin white layer of snow. From here, you can move east into the fiction section."

languageShelf :: Item
languageShelf = item "language shelf" "The shelf is filled with dictionaries and grammar guides. To be honest, you have probably not opened one of them after placing them in the shelf."

beforeExamineLanguageShelf :: GameContext -> (GameContext,Bool)
beforeExamineLanguageShelf gc 
    | shouldFindLatinDictionary = (gc''',True)
    | otherwise = (gc,False) 
  where shouldFindLatinDictionary = gcCarryingItem gc HouseHistoryBook && not (gcFlagIsSet LatinDictionaryFound gc)
        gc'   = gcSetFlag LatinDictionaryFound gc
        gc''  = gcLocAddItem TravelSection LatinDictionary gc'
        gc''' = gc'' { gcResult = "The shelf is filled with dictionaries and grammar guides. Pondering the old book in your hand, you look more closely for a latin dictionary which you are able to find, squeezed between Astrid Stedje's \"Deutsche Sprache gestern und heute\" and an old edition of \"The Oxford Companion to English Literature\"." }

latinDictionary :: Item
latinDictionary = item "latin dictionary" "The cover of this book simply says \"A Latin Dictionary\". \"Well\", you mutter to yourself, \"I guess no more information is needed\"."

travelShelf :: Item
travelShelf = item "travel shelf" "Oh, the books in this shelf. How you have read them, some from cover to cover, others more haphazardly, wishing that you were somewhere other than in this dreary town."

--------------------------------------------------------------------------------------------------------------------
-----
----- Fiction section
-----
--------------------------------------------------------------------------------------------------------------------

fictionSection :: Location
fictionSection = location "fiction section" "You are in the main section of your book shop. This section is filled with literary fiction - shelf after shelf of romance, crime and drama. At one wall, there is a small coffee table and two rickety chairs. This is where you intend to serve your loyal customers a cup of tea or coffee, and maybe a homemade biscuit or two, while they ponder on their purchases or just enjoy the literary ambience. This assumes, of course, that you have any loyal customers. Hardly anyone has used this table since you placed it there, nearly two years ago.<br><br>There are two large windows here, one on each side of the entrance door. Through the windows, you can see the large square, and the beatiful old Gothic cathedral at the other side of it. Tourists are milling about, but as usual nobody seems to notice your fine establishment. On the wall, between the door and one of the windows, hangs a small plaque. Above the door is a small bell to indicate when a customer enters. Too rarely does it sound. From here you can go west to the travel and language section, east to the art section or south to the kitchen."

entranceDoor :: Item
entranceDoor = item "front door" "This is the main entrance to your book shop. You see a sign that says \"Closed\". Luckily, that means that is says \"Open\" on the other side."

beforeEntranceDoorOpen :: GameContext -> (GameContext,Bool)
beforeEntranceDoorOpen = blockAction "As you move toward the door to open it, you realise that you just got here and that it is not yet time for lunch. If ever a customer should venture into your shop, you had better be here."

plaque :: Item
plaque = item "plaque" "This is a small copper plaque, on which is inscribed \"Parswick Books - City Centre merchant of the year 1979\". It is signed by \"The merchant guild of Parswick\". These were better times indeed."

--------------------------------------------------------------------------------------------------------------------
-----
----- Art section
-----
--------------------------------------------------------------------------------------------------------------------

artSection :: Location
artSection = location "art and architecture section" "You are in the art and architecture section. Many of the books in these shelves are of the \"coffee table book\" variety. Through the window, you can see the street outside. Both the street and the red letterbox immediately outside the window are covered by a fine layer of snow. From here you can go west to the main section or south to the history section."

--------------------------------------------------------------------------------------------------------------------
-----
----- History section
-----
--------------------------------------------------------------------------------------------------------------------

historySection :: Location
historySection = location_f "history section" f where
    f gc = "This windowless room contains books on history, a subject that has been dear to you ever since you took over the bookshop. Maybe as a result of this, the room is filled to the brim with books on the subjcect. Along the walls are books in the shelves from floor to ceiling. Two coffee tables are located here, both covered by large stacks of books. The two armchairs standing in one corner are also filled with books. You really ought to get rid of some of the books in here, and clean up the room a bit. If only someone would come in and buy a lot of these books... " ++ if_entrance_known ++ "<br><br>From here you can go north to the art section or south to your office" ++ if_office_key_found
        where if_entrance_known
                  | gcFlagIsSet EntranceIsKnown gc = "You look at the bookshelf on the eastern wall in a way that you have never done before. "
                  | otherwise                      = ""
              if_office_key_found
                  | gcFlagIsSet OfficeKeyFound gc = "." 
                  | otherwise                     = " which you know is filled with even more books. At least you could enter the office if the door wasn't locked and you hadn't misplaced the key. Upon second thought, north seems the only viable option here."

officeDoor :: Item
officeDoor = door "office door" "" OfficeDoorKey

--------------------------------------------------------------------------------------------------------------------
-----
----- Office
-----
--------------------------------------------------------------------------------------------------------------------

office :: Location
office = location_f "office" f where
    f gc = "This small office is dominated by a large desk in its centre. Once this well-crafted mahogany desk was your grandfather's pride, but nowadays its worn surface can hardly be seen under all the paperwork that has gathered on top of it. You don't have to look closely to know that a large portion of the papers are bills, many with a due date too far back into the past for comfort. " ++ if_drawer_introduced ++ "To one side stands a large cabinet which you know is filled with the kind of old stuff that goes into a cabinet that is never opened. Covering the rest of the walls are further book shelves, containing those books you know that you will never be able to sell. " ++ describe_safe ++ "From here, you can go north to the history section."
        where if_drawer_introduced = "In the desk is a drawer. " -- TODO
              describe_safe = "<<safeopen>>An old safe stands in a corner, its door slightly ajar.<<else>>In a corner stands an old safe. What it contains, you don't know. Unfortunately, when your father died very suddenly all those years ago, the combination died with him. For all you know, the solution to your financial problems could be in that sturdy old safe.<<end>> " -- TODO

--------------------------------------------------------------------------------------------------------------------
-----
----- Kitchen
-----
--------------------------------------------------------------------------------------------------------------------

kitchen :: Location
kitchen = location "kitchen" "The cramped kitchen contains only the most essential - a sink and a small fridge. On the wall above the sink is a small cupboard with a plain white door. Here is also a small wooden table and a rickety old chair. This room has a musty smell, but you have decided not to pursue the reason for the pungent odour. Everything in here looks as if it was cheap even when the kitchen was installed many years ago. From here you can go north to the main section or south into the bathroom."

sink :: Item
sink = item_f "sink" f 
    where f gc | gcFlagIsSet WaterCookerIntroduced gc = s
               | otherwise                            = s ++ " On the sink there is a small water cooker."
          s = "The stainless steel sink has a strange coloration at one end. You have a faint memory of pouring some kind of paint into it it an early age. The tap provides cold water only." 

--     = if not (gcFlagIsSet WaterCookerIntroduced gc) then s ++ " On the sink there is a small water cooker." else s 
--          s = "The stainless steel sink has a strange coloration at one end. You have a faint memory of pouring some kind of paint into it it an early age. The tap provides cold water only." 

afterExamineSink :: GameContext -> GameContext
afterExamineSink gc
    | gcFlagIsSet WaterCookerIntroduced gc = gc 
    | otherwise                            = gcLocAddItem Kitchen WaterCooker $ gcSetFlag WaterCookerIntroduced gc

waterCooker :: Item
waterCooker = item "water cooker" "You have used this water cooker to make your tea for at many years. A trusty friend."

fridge :: Item
fridge = item "fridge" "The white fridge is humming slightly. Once in a while it makes a strange coughing noise. You take a peek inside. It is empty, as expected."

cupboard :: Item
cupboard = item "cupboard" "It is a plain white cupboard, typical for a kitchen that has been decorated with economy in mind."

cup :: Item
cup = item "cup" "The cup is white and has the text \"Hotel del Sol, Tenerife\" written on it. As far as you can remember, you have never been to Tenerife."

cupTaken :: GameContext -> GameContext
cupTaken = gcLocIntroduceContainedItem Kitchen OfficeDoorKey Cupboard "When you take the cup, a key appears."

officeDoorKey :: Item
officeDoorKey = item "office door key" "This is the key to the office."

{-
  Table
  -----
  - Examine: Its worn surface is covered by the doodles that you placed there in your childhood, while spending many long hours waiting for your parents to finish their business in the bookshop.

  Chair
  -----
  - Examine: It looks uncomfortable and, having used it on many occasions, you know it is.
-}

--------------------------------------------------------------------------------------------------------------------
-----
----- Game context and game data
-----
--------------------------------------------------------------------------------------------------------------------

startContext :: GameContext
startContext = 
     gcSetVerb OfficeDoor Unlock $
     gcSetVerb EntranceDoor Open $ 
     gcSetVerb LatinDictionary Take $ 
     gcSetVerb WaterCooker Take $ 
     gcSetVerb Cupboard Open $ 
     gcSetVerb Cup Take $
     gcSetVerb OfficeDoorKey Take $
     
     gcLocAddItem HistorySection OfficeDoor $
     gcLocAddItem Office OfficeDoor $

     gcLocAddItem FictionSection EntranceDoor $
     gcLocAddItem FictionSection Plaque $
     gcLocAddItem TravelSection LanguageShelf $
     gcLocAddItem TravelSection TravelShelf $
     gcLocAddItem Kitchen Sink $
     gcLocAddItem Kitchen Fridge $
     gcLocAddContainedItem Kitchen Cup Cupboard $
     gcLocAddItem Kitchen Cupboard $
     gameContext FictionSection "Welcome to the game!"

gd :: GameData
gd = gdAddBeforeApplyFunction Open EntranceDoor beforeEntranceDoorOpen $
     gdAddBeforeApplyFunction Examine LanguageShelf beforeExamineLanguageShelf $
     gdAddAfterApplyFunction Examine Sink afterExamineSink $
     gdAddAfterApplyFunction Take Cup cupTaken $ 

     gdAddItem EntranceDoor entranceDoor $
     gdAddItem Plaque plaque $
     gdAddItem LanguageShelf languageShelf $
     gdAddItem TravelShelf travelShelf $
     gdAddItem LatinDictionary latinDictionary $
     gdAddItem Sink sink $
     gdAddItem WaterCooker waterCooker $
     gdAddItem Fridge fridge $
     gdAddItem Cupboard cupboard $
     gdAddItem OfficeDoorKey officeDoorKey $
     gdAddItem Cup cup $

     gdAddDoor HistorySection S OfficeDoor officeDoor $
     gdAddDoor Office N OfficeDoor officeDoor $

     gdAddConnection TravelSection E FictionSection $
     gdAddConnection FictionSection E ArtSection $
     gdAddConnection ArtSection S HistorySection $
     gdAddConnection HistorySection S Office $
     gdAddConnection FictionSection S Kitchen $

     gdAddLocation TravelSection travelSection $
     gdAddLocation FictionSection fictionSection $
     gdAddLocation ArtSection artSection $
     gdAddLocation HistorySection historySection $
     gdAddLocation Office office $
     gdAddLocation Kitchen kitchen $
     gameData

