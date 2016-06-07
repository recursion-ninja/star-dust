{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick        ((<+>),(<=>),withBorderStyle,str,txt,hLimit,vLimit,viewport)
import qualified Brick.Types                as T
import           Brick.Types  (Widget, ViewportType(Both))
import qualified Brick.Main                 as M
import           Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center
import           Control.Monad (void)
import           Data.Default
import           Data.List     (intercalate)
import           Data.Monoid   ((<>))
import qualified Data.Text                  as Text
import qualified Graphics.Vty as V
import           Lib
--import           Language.Haskell.TH.Syntax (mkName)
--ui :: Widget
--ui = str "Hello, world!"

--main :: IO ()
--main = simpleMain $ totalFrame

totalFrame :: Widget MyName
totalFrame = mapFrame <=> contextFrame

mapFrame :: Widget MyName
mapFrame = viewport vpMap Both
--         . vCenter
--         . hCenter
         . vLimit  60
         . hLimit 250
         $ txt . Text.pack
         $ unlines
         $ replicate 3 (intercalate ("\n"<> replicate 250 ' ' <>"\n")
                          [ concat $ replicate 25 "0123456789"
                          , concat $ replicate 25 "1234567890"
                          , concat $ replicate 25 "2345678901"
                          , concat $ replicate 25 "3456789012"
                          , concat $ replicate 25 "4567890123"
                          , concat $ replicate 25 "5678901234"
                          , concat $ replicate 25 "6789012345"
                          , concat $ replicate 25 "7890123456"
                          , concat $ replicate 25 "8901234567"
                          , concat $ replicate 25 "9012345678"
                          ]
                          <> replicate 250 ' ' <>"\n"
                        )

contextFrame :: Widget MyName
contextFrame
  = withBorderStyle BS.unicode
  $ borderWithLabel (str "Stats")
  $ hCenter
  $ vLimit 4
  $ ship <+> vBorder <+> pilot <+> vBorder <+> history <+> vBorder <+> exp
  where
    ship = withBorderStyle BS.unicode
      . vLimit 4
      . vCenter
      $ txt shipInfo
    pilot = withBorderStyle BS.unicode
      . vLimit 4
      . vCenter
      $ txt pilotInfo
    history = withBorderStyle BS.unicode
      . vLimit 4
      . vCenter
      $ txt historyInfo
    exp =  withBorderStyle BS.unicode
      . vLimit 4
      . vCenter
      $ txt expInfo
    shipInfo  = Text.unlines $ pad <$>
      [ "Shld:  8"
      , "Hull:  8"
      , "Comp:  1"
      , "Powr:  6"
      ]
    pilotInfo = Text.unlines $ pad <$>
      [ "PtDf:  4"
      , "Mnvr:  4"
      , "Acry:  5"
      , "Snsr:  1"
      ]
    historyInfo = Text.unlines $ pad <$>
      [ "Kills  :  17"
      , "Deaths :   1"
      , "Battles:   8"
      , "Rank   :   4"
      ]
    expInfo    = Text.unlines $ pad <$>
      [ " UniqueName "
      , " Experiance "
      , "    341/400 "
      , "[========- ]"
      ]

pad x = " " <> x <> " "

data MyName = VPMap deriving (Eq, Ord, Show)

--vpMap :: T.Name
vpMap = VPMap --() -- undefined -- mkName "mapViewport"

drawUi :: () -> [Widget MyName]
drawUi = const [totalFrame]

vp3Scroll :: M.ViewportScroll MyName
vp3Scroll = M.viewportScroll vpMap

appEvent :: () -> V.Event -> T.EventM MyName (T.Next ())
appEvent _ (V.EvKey V.KDown  []) = M.vScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KUp    []) = M.vScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KRight []) = M.hScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KLeft  []) = M.hScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KEsc   []) = M.halt ()
appEvent _ _ = M.continue ()

app :: M.App () V.Event MyName
app = M.App { M.appDraw         = drawUi
            , M.appStartEvent   = pure
            , M.appHandleEvent  = appEvent
            , M.appAttrMap      = const def
            , M.appLiftVtyEvent = id
            , M.appChooseCursor = M.neverShowCursor
            }

main :: IO ()
main = void $ M.defaultMain app ()
