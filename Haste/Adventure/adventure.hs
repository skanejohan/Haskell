module Main where

import Prelude hiding (take, drop)

import Haste
import Haste.DOM
import Haste.Events
import Haste.JSON
import Haste.Serialize
import Haste.LocalStorage

import Types
import Framework
import Data

import Include.IncludeList as IL

-- State and Local storage

instance Serialize GameContext where
  toJSON = Str . toJSString . show
  parseJSON (Str jss) = return . read  $ fromJSStr jss

getGameContext :: IO GameContext
getGameContext = do
    es <- getItem "GameContext"
    case es of
        Left _ -> return startContext
        Right s -> return s

setGameContext :: GameContext -> IO ()
setGameContext = setItem "GameContext"

-- General functions

addButton :: Elem -> String -> IO () -> IO HandlerInfo
addButton parent caption function = do
    b <- newElem "button"
    set b [ prop "innerHTML" =: caption ]
    appendChild parent b
    onEvent b Click $ const function

dummyOp :: Elem -> IO () -> IO HandlerInfo
dummyOp div function = do
    setProp div "innerHTML" ""
    b <- newElem "button"
    appendChild div b
    onEvent b Click $ const function

-- Main

main :: IO ()
main = withElems ["restartBtn","eastBtn","southBtn","westBtn","northBtn","messageDiv", "locationDiv","locationItemsDiv","carriedItemsDiv","dummyDiv"] (ui startContext)

ui :: GameContext -> [Elem] -> IO ()
ui gc elems@[restartBtn,eastBtn,southBtn,westBtn,northBtn,messageDiv,locationDiv,locationItemsDiv,carriedItemsDiv,dummyDiv] = do
    onEvent eastBtn  Click $ const (update elems $ move gd E) 
    onEvent southBtn Click $ const (update elems $ move gd S)
    onEvent westBtn  Click $ const (update elems $ move gd W)
    onEvent northBtn Click $ const (update elems $ move gd N)
    onEvent restartBtn  Click $ const (update elems $ const gc)
    setGameContext gc
    update elems id

update :: [Elem] -> (GameContext -> GameContext) -> IO ()
update elems@[restartBtn,eastBtn,southBtn,westBtn,northBtn,messageDiv,locationDiv,locationItemsDiv,carriedItemsDiv,dummyDiv] f = do
    oldGc <- getGameContext
    setGameContext $ f oldGc
    newGc <- getGameContext
    setProp messageDiv "innerHTML" (gcResult newGc)
    setProp locationDiv "innerHTML" (describeCurrentLocation gd newGc)
    listItemsAndDoors locationItemsDiv dummyDiv newGc (update elems)
    --listDoors rightBottomDiv dummyDiv newGc (update elems)
    listCarriedItems carriedItemsDiv dummyDiv newGc (update elems)
    setClass eastBtn "enabled" (gdLocHasExit gd (gcLocation newGc) E)
    setClass southBtn "enabled" (gdLocHasExit gd (gcLocation newGc) S)
    setClass westBtn "enabled" (gdLocHasExit gd (gcLocation newGc) W)
    setClass northBtn "enabled" (gdLocHasExit gd (gcLocation newGc) N)

{-
listLocationItems :: Elem -> Elem -> GameContext -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo
listLocationItems div dummyDiv gc f = do
    setProp div "innerHTML" "You see the following items here:<br><br>"
    listItems div dummyDiv (gcLocAllItems gc (gcLocation gc)) f
  where
    listItems :: Elem -> Elem -> [ItemId] -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo
    listItems div dummyDiv (i:is) f = do 
        listItem div dummyDiv i f
        listItems div dummyDiv is f
    listItems div dummyDiv [] f = do 
        dummyOp dummyDiv (f id)
    listItem :: Elem -> Elem -> ItemId -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo    
    listItem div dummyDiv item_id f = do
        d <- newElem "div"
        appendChild div d
        text <- newTextElem (describeItem gd gc item_id)
        appendChild d text
        addButton d "Examine" (f $ apply gd Examine item_id)
        if gcItemHasVerb gc item_id Close
          then addButton d "Close" (f $ apply gd Close item_id)
          else dummyOp dummyDiv (f id)
        if gcItemHasVerb gc item_id Open
          then addButton d "Open" (f $ apply gd Open item_id)
          else dummyOp dummyDiv (f id)
        if gcItemHasVerb gc item_id Take
          then addButton d "Take" (f $ apply gd Take item_id)
          else dummyOp dummyDiv (f id)
-}
--getItemsAndDoorsWithActions :: GameData -> GameContext -> [(String,[(Verb,GameContext -> GameContext)])]

listItemsAndDoors :: Elem -> Elem -> GameContext -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo
listItemsAndDoors div dummyDiv gc f = do
    setProp div "innerHTML" "You see the following here:<br><br>"
    listItems div dummyDiv (getItemsWithActions gd gc) f
  where
    listItems :: Elem -> Elem -> [(String,[(Verb,GameContext -> GameContext)])] -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo
    listItems div dummyDiv (i:is) f = do 
        listItem div dummyDiv i f
        listItems div dummyDiv is f
    listItems div dummyDiv [] f = do 
        dummyOp dummyDiv (f id)
    listItem :: Elem -> Elem -> (String,[(Verb,GameContext -> GameContext)]) -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo    
    listItem div dummyDiv (s,actions) f = do
        d <- newElem "div"
        appendChild div d
        text <- newTextElem s
        appendChild d text
        listSubItems d dummyDiv actions f
    listSubItems :: Elem -> Elem -> [(Verb,GameContext -> GameContext)] -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo    
    listSubItems div dummyDiv (a:as) f = do
        listSubItem div dummyDiv a f
        listSubItems div dummyDiv as f
    listSubItems div dummyDiv [] f = do
        dummyOp dummyDiv (f id)
    listSubItem :: Elem -> Elem -> (Verb,GameContext -> GameContext) -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo    
    listSubItem div dummyDiv (v,fn) f = 
        addButton div (show v) (f $ fn)
        --dummyOp dummyDiv (f id)
        --addButton d "Examine" (f $ apply gd Examine item_id)
        --if gcItemHasVerb gc item_id Close
        --  then addButton d "Close" (f $ apply gd Close item_id)
        --  else dummyOp dummyDiv (f id)
        --if gcItemHasVerb gc item_id Open
        --  then addButton d "Open" (f $ apply gd Open item_id)
        --  else dummyOp dummyDiv (f id)
        --if gcItemHasVerb gc item_id Take
        --  then addButton d "Take" (f $ apply gd Take item_id)
        -- else dummyOp dummyDiv (f id)

listCarriedItems :: Elem -> Elem -> GameContext -> ((GameContext -> GameContext) -> IO ()) -> IO HandlerInfo
listCarriedItems div dummyDiv gc f = do
    setProp div "innerHTML" "You carry the following:<br><br>"
    listItems div dummyDiv (IL.flatten $ gcInventory gc) f
  where
    listItems div dummyDiv (i:is) f = do 
        listItem div dummyDiv i f
        listItems div dummyDiv is f
    listItems div dummyDiv [] f = do 
        dummyOp dummyDiv (f id)
    listItem div dummyDiv item_id f = do
        d <- newElem "div"
        appendChild div d
        text <- newTextElem (describeItem gd gc item_id)
        appendChild d text
        addButton d "Drop" (f $ apply gd Drop item_id)
        addButton d "Examine" (f $ apply gd Examine item_id)
