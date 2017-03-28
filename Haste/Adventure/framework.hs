module Framework where

import Types 
import Include.IncludeList as IL

import qualified Data.Map as M
import qualified Data.Set as S

data Direction = N | E | S | W | U | D deriving (Eq, Ord, Show, Read)

opposite :: Direction -> Direction
opposite N = S
opposite E = W
opposite S = N
opposite W = E
opposite U = D
opposite D = U

data Verb = Take 
          | Drop
          | Examine
          | Open
          | Close
          | Lock
          | Unlock deriving (Eq, Ord, Show, Read)

data Item = Item
    { iName        :: String
    , iDescription :: GameContext -> String
    , iKey         :: Maybe ItemId
    }

data Location = Location 
    { lShort :: String
    , lLong  :: GameContext -> String
    }

data GameData = GameData
    { gdItems                :: M.Map ItemId                 Item
    , gdLocations            :: M.Map LocationId             Location
    , gdExits                :: M.Map (LocationId,Direction) LocationId
    , gdMoveFunctions        :: M.Map LocationId             (GameContext -> GameContext)
    , gdBeforeApplyFunctions :: M.Map (Verb,ItemId)          (GameContext -> (GameContext,Bool))
    , gdAfterApplyFunctions  :: M.Map (Verb,ItemId)          (GameContext -> GameContext)
    , gdLocationDoors        :: M.Map (LocationId,Direction) ItemId
    }

data GameContext = GameContext
    { gcLocation   :: LocationId
    , gcInventory  :: IncludeList ItemId
    , gcResult     :: String
    , gcFlags      :: S.Set Flag
    , gcItems      :: M.Map LocationId (IncludeList ItemId)
    , gcItemVerbs  :: M.Map ItemId (S.Set Verb)
    } deriving (Show, Read)

fromMaybe :: a -> Maybe a -> a
fromMaybe def fn = case fn of Just a -> a
                              Nothing -> def

item :: String -> String -> Item
item n d = Item { iName = n, iDescription = const d, iKey = Nothing }

item_f :: String -> (GameContext -> String) -> Item
item_f n f = Item { iName = n, iDescription = f, iKey = Nothing }

door :: String -> String -> ItemId -> Item
door n d k = Item { iName = n, iDescription = const d, iKey = Just k }

door_f :: String -> (GameContext -> String) -> ItemId -> Item
door_f n f k = Item { iName = n, iDescription = f, iKey = Just k }

location :: String -> String -> Location 
location s l = Location { lShort = s, lLong = const l }

location_f :: String -> (GameContext -> String) -> Location 
location_f s f = Location { lShort = s, lLong = f }

gameData :: GameData
gameData = GameData 
    { gdItems = M.empty
    , gdLocations = M.empty
    , gdExits = M.empty
    , gdMoveFunctions = M.empty
    , gdBeforeApplyFunctions = M.empty
    , gdAfterApplyFunctions = M.empty
    , gdLocationDoors = M.empty
    }

gameContext :: LocationId -> String -> GameContext
gameContext l s = GameContext
    { gcLocation   = l
    , gcInventory  = empty
    , gcResult     = s
    , gcFlags      = S.empty
    , gcItems      = M.empty
    , gcItemVerbs  = M.empty
    }

-------------------------------------------------------------------------------
-----
----- Functions on GameContext
-----
-------------------------------------------------------------------------------

gcItemHasVerb :: GameContext -> ItemId -> Verb -> Bool
gcItemHasVerb gc i v = case M.lookup i (gcItemVerbs gc) of
    Just vs -> S.member v vs
    Nothing -> False

gcItemAllVerbs :: GameContext -> ItemId -> [Verb]
gcItemAllVerbs gc i = case M.lookup i (gcItemVerbs gc) of
    Just vs -> Examine : S.elems vs
    Nothing -> [Examine]

gcSetVerb :: ItemId -> Verb -> GameContext -> GameContext
gcSetVerb i v gc = 
    let verbs = fromMaybe S.empty (M.lookup i (gcItemVerbs gc))
    in gc { gcItemVerbs = M.insert i (S.insert v verbs) (gcItemVerbs gc) }

gcClearVerb :: ItemId -> Verb -> GameContext -> GameContext
gcClearVerb i v gc =
    let verbs = fromMaybe S.empty (M.lookup i (gcItemVerbs gc))
    in gc { gcItemVerbs = M.insert i (S.delete v verbs) (gcItemVerbs gc) }

gcReplaceVerb :: ItemId -> Verb -> Verb -> GameContext -> GameContext
gcReplaceVerb i v1 v2 gc = gcSetVerb i v2 $ gcClearVerb i v1 gc

gcLocHasItem :: GameContext -> LocationId -> ItemId -> Bool
gcLocHasItem gc l i = case M.lookup l (gcItems gc) of 
    Just il -> IL.elem i il
    Nothing -> False

gcLocAllItems :: GameContext -> LocationId -> [ItemId]
gcLocAllItems gc l = let items = fromMaybe empty $ M.lookup l (gcItems gc)
                     in flatten_cond (\i -> not $ gcItemHasVerb gc i Open) items

gcLocAddItem :: LocationId -> ItemId -> GameContext -> GameContext
gcLocAddItem l i gc = let itemList = case M.lookup l (gcItems gc) of 
                                         Just il -> IL.add i il
                                         Nothing -> IL.add i IL.empty
                      in gc { gcItems = M.insert l itemList $ gcItems gc }

gcLocAddContainedItem :: LocationId -> ItemId -> ItemId -> GameContext -> GameContext
gcLocAddContainedItem l i cont gc = let itemList = case M.lookup l (gcItems gc) of 
                                                       Just il -> IL.add_to i cont il
                                                       Nothing -> IL.add_to i cont IL.empty
                                    in gc { gcItems = M.insert l itemList $ gcItems gc }

gcLocIntroduceItem :: LocationId -> ItemId -> String -> GameContext -> GameContext
gcLocIntroduceItem l i s gc
    | gcLocHasItem gc l i = gc
    | otherwise           = let gc' = gcLocAddItem l i gc in gc' { gcResult = s }

gcLocIntroduceContainedItem :: LocationId -> ItemId -> ItemId -> String -> GameContext -> GameContext
gcLocIntroduceContainedItem l i cont s gc
    | gcLocHasItem gc l i = gc
    | otherwise           = let gc' = gcLocAddContainedItem l i cont gc in gc' { gcResult = s }

gcLocRemoveItem :: GameContext -> LocationId -> ItemId -> GameContext
gcLocRemoveItem gc l i = let itemList = case M.lookup l (gcItems gc) of 
                                            Just il -> IL.delete i il
                                            Nothing -> IL.empty
                         in gc { gcItems = M.insert l itemList $ gcItems gc }

gcCarryingItem :: GameContext -> ItemId -> Bool 
gcCarryingItem gc i = IL.elem i $ gcInventory gc

gcPickupItem :: GameContext -> ItemId -> GameContext
gcPickupItem gc i = gc { gcInventory = IL.add i (gcInventory gc) }

gcDropItem :: GameContext -> ItemId -> GameContext
gcDropItem gc i = gc { gcInventory = IL.delete i (gcInventory gc) }

gcFlagIsSet :: Flag -> GameContext -> Bool
gcFlagIsSet f gc = S.member f (gcFlags gc)

gcSetFlag :: Flag -> GameContext -> GameContext
gcSetFlag f gc = gc { gcFlags = S.insert f (gcFlags gc) }

gcClearFlag :: Flag -> GameContext -> GameContext
gcClearFlag f gc = gc { gcFlags = S.delete f (gcFlags gc) }

blockAction :: String -> GameContext -> (GameContext,Bool)
blockAction s gc = (gc { gcResult = s },True)

-------------------------------------------------------------------------------
-----
----- Functions on GameData
-----
----- Note: the functions returning a modified GameData object are used for 
----- construction of the inital object. Once in the game, this object does
----- not change.
-----
-------------------------------------------------------------------------------

gdAddItem :: ItemId -> Item -> GameData -> GameData
gdAddItem i it gd = gd { gdItems = M.insert i it (gdItems gd) }

gdAddLocation :: LocationId -> Location -> GameData -> GameData
gdAddLocation i l gd = gd { gdLocations = M.insert i l (gdLocations gd) }

gdAddBeforeApplyFunction :: Verb -> ItemId -> (GameContext -> (GameContext,Bool)) -> GameData -> GameData
gdAddBeforeApplyFunction v i f gd = gd { gdBeforeApplyFunctions = M.insert (v,i) f (gdBeforeApplyFunctions gd) }

gdAddAfterApplyFunction :: Verb -> ItemId -> (GameContext -> GameContext) -> GameData -> GameData
gdAddAfterApplyFunction v i f gd = gd { gdAfterApplyFunctions = M.insert (v,i) f (gdAfterApplyFunctions gd) }

gdAddExit :: LocationId -> Direction -> LocationId -> GameData -> GameData
gdAddExit s d t gd = gd { gdExits = M.insert (s,d) t (gdExits gd) }

gdAddConnection :: LocationId -> Direction -> LocationId -> GameData -> GameData
gdAddConnection l1 d1 l2 gd = gdAddExit l1 d1 l2 $ gdAddExit l2 (opposite d1) l1 gd

gdLocHasExit :: GameData -> LocationId -> Direction -> Bool
gdLocHasExit gd l d = M.member (l,d) (gdExits gd)

gdLocTarget :: GameData -> LocationId -> Direction -> LocationId
gdLocTarget gd l d = fromMaybe l (M.lookup (l, d) (gdExits gd))

gdItemName :: GameData -> ItemId -> String
gdItemName gd i = case M.lookup i $ gdItems gd of
    Just it -> iName it
    Nothing -> ""

gdItemDescription :: GameData -> GameContext -> ItemId -> String
gdItemDescription gd gc i = case M.lookup i $ gdItems gd of
    Just it -> iDescription it gc
    Nothing -> ""

gdLocationShort :: GameData -> LocationId -> String
gdLocationShort gd l = case M.lookup l $ gdLocations gd of
    Just loc -> lShort loc
    Nothing  -> ""

gdLocationLong :: GameData -> LocationId -> GameContext -> String
gdLocationLong gd l = case M.lookup l $ gdLocations gd of
    Just loc -> lLong loc
    Nothing  -> const ""

gdMoveFunction :: GameData -> LocationId -> (GameContext -> GameContext)
gdMoveFunction gd l = fromMaybe id (M.lookup l (gdMoveFunctions gd))

gdBeforeApplyFunction :: GameData -> Verb -> ItemId -> (GameContext -> (GameContext,Bool))
gdBeforeApplyFunction gd v i = fromMaybe (\gc -> (gc,False)) (M.lookup (v,i) (gdBeforeApplyFunctions gd))

gdAfterApplyFunction :: GameData -> Verb -> ItemId -> (GameContext -> GameContext)
gdAfterApplyFunction gd v i = fromMaybe id (M.lookup (v,i) (gdAfterApplyFunctions gd))

gdAddDoor :: LocationId -> Direction -> ItemId -> Item -> GameData -> GameData
gdAddDoor l d i door gd = gd { gdLocationDoors = M.insert (l,d) i (gdLocationDoors gd)
                             , gdItems = M.insert i door (gdItems gd) 
                             }

gdLocAllDoors :: GameData -> LocationId -> [ItemId]
gdLocAllDoors gd l = f N ++ f E ++ f S ++ f W ++ f U ++ f D
    where f d = case M.lookup (l,d) (gdLocationDoors gd) of Just id -> [id]
                                                            Nothing -> []

-------------------------------------------------------------------------------
-----
----- Functions on GameData + GameContext
-----
-------------------------------------------------------------------------------

describeItem :: GameData -> GameContext -> ItemId -> String
describeItem gd gc i
      | gcItemHasVerb gc i Open  = gdItemName gd i ++ " (which is closed)"
      | gcItemHasVerb gc i Close = gdItemName gd i  ++ " (which is open)"
      | otherwise                = gdItemName gd i

describeCurrentLocation :: GameData -> GameContext -> String
describeCurrentLocation gd gc = gdLocationLong gd (gcLocation gc) gc

getDoor :: GameData -> LocationId -> Direction -> Maybe ItemId
getDoor gd l d = M.lookup (l,d) (gdLocationDoors gd)

hasLockedDoor :: GameData -> LocationId -> Direction -> GameContext -> Bool
hasLockedDoor gd l d gc = case getDoor gd l d of     --getDoorState gd gc l d == DsLocked
    Just doorId -> gcItemHasVerb gc doorId Unlock
    Nothing     -> False 

hasClosedDoor :: GameData -> LocationId -> Direction -> GameContext -> Bool
hasClosedDoor gd l d gc = case getDoor gd l d of     --getDoorState gd gc l d == DsLocked
    Just doorId -> gcItemHasVerb gc doorId Open
    Nothing     -> False 

examineItem :: GameData -> ItemId -> GameContext -> String
examineItem gd i gc
      | gcItemHasVerb gc i Open  = s ++ " It is closed."
      | gcItemHasVerb gc i Close = s ++ " It is open."
      | otherwise                = s
  where s | desc == "" = "You see nothing special about the " ++ gdItemName gd i ++ "."
          | otherwise  = desc
        desc = gdItemDescription gd gc i

take :: GameData -> ItemId -> GameContext -> GameContext
take gd i gc
    | gcLocHasItem gc (gcLocation gc) i = let gc'  = gcLocRemoveItem gc (gcLocation gc) i
                                              gc'' = gcPickupItem gc' i
                                          in gc'' { gcResult = "You take the " ++ gdItemName gd i ++ "."} 
    | otherwise                         = gc { gcResult = "You can't see that here!" } 

drop :: GameData -> ItemId -> GameContext -> GameContext
drop gd i gc
    | gcCarryingItem gc i = let gc'  = gcLocAddItem (gcLocation gc) i gc
                                gc'' = gcDropItem gc' i
                            in gc'' { gcResult = "You drop the " ++ gdItemName gd i }
    | otherwise           = gc { gcResult = "You don't have that!" } 

open :: GameData -> ItemId -> GameContext -> GameContext 
open gd i gc = let gc' = gcReplaceVerb i Open Close gc
               in gc' { gcResult = "You open the " ++ gdItemName gd i ++ "." }

close :: GameData -> ItemId -> GameContext -> GameContext 
close gd i gc = let gc' = gcReplaceVerb i Close Open gc
                in gc' { gcResult = "You close the " ++ gdItemName gd i ++ "." }

examine :: GameData -> ItemId -> GameContext -> GameContext
examine gd i gc = gc { gcResult = examineItem gd i gc }

move :: GameData -> Direction -> GameContext -> GameContext
move gd d gc
    | hasLockedDoor gd (gcLocation gc) d gc = gc { gcResult = "The door is locked." }
    | hasClosedDoor gd (gcLocation gc) d gc = let gc'   = openDoor gc
                                                  gc''  = gc' { gcLocation = gdLocTarget gd (gcLocation gc) d } 
                                                  gc''' = gc'' { gcResult = "First opening the door, you move to the " ++ gdLocationShort gd (gcLocation gc') ++ "." }
                                              in gdMoveFunction gd (gcLocation gc''') gc'''
    | gdLocHasExit gd (gcLocation gc) d = let gc'  = gc  { gcLocation = gdLocTarget gd (gcLocation gc) d }
                                              gc'' = gc' { gcResult = "You move to the " ++ gdLocationShort gd (gcLocation gc') ++ "." }
                                          in gdMoveFunction gd (gcLocation gc'') gc''
    | otherwise                         = gc { gcResult = "There is no exit in that direction!" } 
    where openDoor c = case getDoor gd (gcLocation c) d of
                                   Just doorId -> gcReplaceVerb doorId Open Close c
                                   Nothing     -> c

apply :: GameData -> Verb -> ItemId -> GameContext -> GameContext
apply gd v i gc = let (gc',b) = gdBeforeApplyFunction gd v i gc
                  in if b then gc' else gdAfterApplyFunction gd v i (apply' v i gc')
    where apply' Examine = Framework.examine gd
          apply' Close   = Framework.close gd
          apply' Open    = Framework.open gd
          apply' Take    = Framework.take gd
          apply' Drop    = Framework.drop gd


-- View model:

getItemsWithActions :: GameData -> GameContext -> [(String,[(Verb,GameContext -> GameContext)])]
getItemsWithActions gd gc = map f $ gcLocAllItems gc (gcLocation gc)
    where f i = let verbs = gcItemAllVerbs gc i
                    pairs = map (\v -> (v,apply gd v i)) verbs
                in (describeItem gd gc i,pairs)
