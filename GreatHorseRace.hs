{- Great Horse Race in Haskell, using WX. -}

-- TODO: just widget sizing & colours, I think.

module Main where

import Graphics.UI.WX

import Data.Array as DA
import System.Random as SR

------------------------------------------------------------------------------
--
-- an OVar (Observable Var) is a Var which invokes attached actions
-- when it is updated.
--
------------------------------------------------------------------------------
data OVar a = OVar (Var a) (Var [a -> IO ()])

mkVar :: a -> IO (OVar a)
mkVar v = do
    state <- variable [ value := v ]
    actions <- variable [ value := [] ]
    return $ OVar state actions

attachVar :: OVar a -> (a -> IO ()) -> IO ()
attachVar (OVar state actions) action = do
    set actions [ value :~ (action :) ]
    -- notify initial state?
    v <- get state value
    action v

getVar :: OVar a -> IO a
getVar (OVar state _) = get state value

setVar :: OVar a -> a -> IO ()
setVar (OVar state actions) v = do
    set state [ value := v ]
    actions' <- get actions value
    sequence_ $ map ($ v) actions'

updateVar :: OVar a -> (a -> a) -> IO ()
updateVar (OVar state actions) f = do
    v <- get state value
    let v' = f v
    set state [ value := v' ]
    actions' <- get actions value
    sequence_ $ map ($ v') actions'


--
-- things which can be shown or hidden
--
class Visibility a where
    setVisibility :: a -> Bool -> IO ()

configureWidgets :: (Visibility a) =>
		    IO a
                 -> OVar [a]
                 -> OVar [a]
                 -> Int
                 -> IO ()
configureWidgets widgetMaker activeV spareV nReq = do
    active <- getVar activeV
    spare <- getVar spareV
    let nActive = length active
        nSpare = length spare

    -- create as many new widgets as we need (maybe none)
    new <- sequence $ replicate (nReq - nActive - nSpare) widgetMaker
    let active' = take nReq (active ++ spare ++ new)
        spare'  = drop nReq (active ++ spare ++ new)

    -- show/hide them
    sequence_ $ map (\w -> setVisibility w False) spare'
    sequence_ $ map (\w -> setVisibility w True) active'

    -- update
    setVar spareV spare'
    setVar activeV active'


------------------------------------------------------------------------------
--
-- a Die
--
------------------------------------------------------------------------------

data Die = Die (OVar Int)               -- number
               (Bool -> IO ())          -- set visible
               (Layout)                 -- layout (!)

mkDie :: Window a
      -> IO Die
mkDie parentW = do
    numberV <- mkVar 1

    numberC <- staticText parentW [ text := "",
                                    fontSize := 16,
                                    fontWeight := WeightBold,
                                    visible := False ]

    attachVar numberV (\v -> do set numberC [ text := show v ])

    return $ Die numberV 
                 (\v -> set numberC [ visible := v ])
                 (widget numberC)

instance Visibility Die where
    setVisibility (Die _ setVisible _) v = setVisible v

-- trivial helpers
setDie :: Die -> Int -> IO ()
setDie (Die d _ _) x = setVar d x

dieLayout (Die _ _ layout) = layout

------------------------------------------------------------------------------
--
-- die rolling
--
------------------------------------------------------------------------------

-- routines for working out number of ways for getting each outcome

collapse zs xs = [(z, sum $ map snd $ filter (\(x,y) -> x == z) xs) | z <- zs]

ways :: Int -> [Int] -> [(Int,Int)]
ways 1 w = zip [1..] w
ways n w =
    let m = sum w
        pairs = concat [ [ (x1 + x2, y1 * y2) | (x1,y1) <- zip [1..] w,
                                                (x2,y2) <- ways (n-1) w,
                                                x1 + x2 == z ]
                         | z <- [1 .. (n * m)] ]
    in
        collapse [n .. (n * (length w))] pairs

maxways n w = foldl max 0 (map snd $ ways n w)

-- abstractions

type Roller1 = (Int, DA.Array Int Int)
mkRoller1 :: [Int] -> Roller1
mkRoller1 weights =
    let slots = concatMap (\(x,y) -> replicate x y) (zip weights [1..])
        nSlots = length slots in
    (nSlots, DA.listArray (0, nSlots - 1) slots)

rollOne :: Roller1 -> IO Int
rollOne (nSlots,slots) = do
    i <- SR.getStdRandom (SR.randomR (0, nSlots - 1))
    return $ slots DA.! i

data Roller = Roller Int [Int] Roller1

mkRoller n w = Roller n w (mkRoller1 w)
rollDice :: Roller -> IO [Int]
rollDice (Roller n _ r) = sequence (replicate n (rollOne r))

minOutcome (Roller n _ _) = n
maxOutcome (Roller n w _) = n * (length w)
nOutcomes r = maxOutcome r - minOutcome r + 1
outcomes (Roller n w _) = ways n w
likelyRolls (Roller n w _) finish =
    finish * ((sum w) `pow` n) `div` (maxways n w)
    where pow x y = foldl (*) 1 (replicate y x)

------------------------------------------------------------------------------
--
-- a Horse
--
------------------------------------------------------------------------------

data HorseVars = HorseVars (OVar Int)   -- number
                           (OVar Int)   -- location
                           (OVar Int)   -- front

data HorseWidgets = HorseWidgets (Button ())
                                 (Window ())
                                 (StaticText ())

data Horse = Horse HorseVars HorseWidgets

colList = map (\(r,g,b) -> rgb r g b) 
              [ (255,0,0), (0,255,0), (0,0,255), 
                (255,255,0), (255,0,255), (0,255,255) ]
nCol = length colList
colArray = listArray (0, nCol - 1) colList

hcol n = colArray ! (n `mod` nCol)

-- TODO remove this bodge - set width correctly
showLocation x = reverse $ take 4 $ reverse $ "    " ++ show x

mkHorse :: Window a
        -> OVar Int
        -> OVar Int
        -> IO Horse
mkHorse parentW finishV frontV = do
    numberV <- mkVar 0
    locationV <- mkVar 0

    let drawHorse numberV locationV finishV dc view = do
        number <- getVar numberV
        location <- getVar locationV
        finish <- getVar finishV
        let viewS = rectSize view
            (viewW, viewH) = (sizeW viewS, sizeH viewS)
            horseH = viewH `div` 3
            horseW = horseH * 2 

        set dc [ brushColor := hcol number ]

        drawRect dc 
                    (rect (point ((viewW - horseW) * location `div` finish)
                                        (viewH `div` 3))
                          (sz horseW horseH))
                    []

    advanceC <- button parentW [ text := "",
                                 on command := updateVar locationV (+1),
                                 fontSize := 16,
                                 fontWeight := WeightBold,
                                 visible := False ]
    graphicC <- window parentW [ on paint := drawHorse numberV locationV finishV,
                                 visible := False ]
    locationC <- staticText parentW [ text := showLocation 0,
                                      fontSize := 16,
                                      fontWeight := WeightBold,
                                      visible := False ]

    attachVar numberV (\v -> set advanceC [ text := show v ])
    attachVar locationV (\v -> do set locationC [ text := showLocation v]
                                  refresh graphicC)
    attachVar finishV (\v -> refresh graphicC)
    return $ Horse (HorseVars numberV locationV frontV)
                   (HorseWidgets advanceC graphicC locationC)


-- some trivial helpers

numberHorse :: Int -> Horse -> IO ()
numberHorse n (Horse (HorseVars number _ _) _) = setVar number n

advanceIfHorse :: Int -> Horse -> IO ()
advanceIfHorse n (Horse (HorseVars numberV locationV frontV) _) = do
    number <- getVar numberV
    if number == n then updateVar locationV (+1)
                   else return ()
    location <- getVar locationV
    updateVar frontV (max location)

restartHorse :: Horse -> IO ()
restartHorse (Horse (HorseVars _ locationV _) _) = setVar locationV 0

horseLayout :: Horse -> [Layout]
horseLayout (Horse _ (HorseWidgets advanceC graphicC locationC)) =
    [ vfill $ widget advanceC,
       fill $ widget graphicC,
      vfill $ widget locationC ]

instance Visibility Horse where
    setVisibility (Horse _ (HorseWidgets advanceC graphicC locationC)) v = do
        set advanceC [ visible := v ]
        set graphicC [ visible := v ]
        set locationC [ visible := v ]

------------------------------------------------------------------------------
--
-- Main Window
--
------------------------------------------------------------------------------

data Pref = Pref {
    p_finish      :: OVar Int,
    p_duration    :: OVar Int,
    p_roller      :: OVar Roller
}

data State = State {
    s_horses      :: OVar [Horse],
    s_spareHorses :: OVar [Horse],
    s_front       :: OVar Int,
    s_count       :: OVar Int,
    s_dice        :: OVar [Die],
    s_spareDice   :: OVar [Die]
}

data Actions = Actions {
    a_roll        :: IO (),
    a_start       :: Int -> IO (),
    a_stop        :: IO (),
    a_restart     :: IO ()
}

createGui :: Frame ()
          -> Pref
          -> State 
          -> Actions
          -> IO ()
createGui f p s a = do
         
    -- create file menu  
    file   <- menuPane      [text := "&File"]
    quit   <- menuQuit file [help := "Quit the demo", on command := close f]

    -- create Help menu
    hlp    <- menuHelp      []
    about  <- menuAbout hlp [help := "About The Great Horse Race"]

    -- create statusbar field
    --status <- statusField   [text := "Welcome to wxHaskell"]

    -- set the statusbar and menubar
    set f [ --statusBar := [status],
            menuBar   := [file,hlp]
            -- put the menu event handler for an about box on the frame.
          ,on (menu about) := infoDialog f "The Great Horse Race v0.6 "
            ( concat [
                    "Copyright (c) 2004 Simon J. Guest\n\n\
\To change the race length, you type in a new number and press return.\n\n\
\The standard race is run with two fair 6-sided dice.\n\n\
\The mystery race is run with the following probabilities :-\n\
\0.08, 0.15, 0.10, 0.08, 0.05, 0.12, 0.16, 0.06, 0.18, 0.02\n\n\
\This software is licensed under the GNU General Public License"] )
          ]

    -- control panel
    cp <- panel f []

    rollB <- button cp [ text := "Roll",
                         on command := a_roll a ]
    startB <- button cp [ text := "Start",
                          on command := do t <- getVar (p_duration p)
                                           (a_start a) t ]
    stopB <- button cp [ text := "Stop",
                         on command := a_stop a ]
    restartB <- button cp [ text := "Restart",
                            on command := a_restart a ]
    standardB <- button cp [ text := "Standard",
                             on command := setVar (p_roller p) $ 
                                                  mkRoller 2 (replicate 6 1)]
    mysteryB <- button cp [ text := "Mystery",
                            on command := setVar (p_roller p) $ mkRoller 1
                                [8,15,10,8,5,12,16,6,18,2] ]
    let buttons = [ rollB, 
                    startB, stopB, restartB, 
                    standardB, mysteryB ]

    finishC <- textEntry cp [ text := "ABCDEFG",
                              processEnter := True ]
    set finishC [ on command := do v <- get finishC text
                                   case (reads :: ReadS Int) v of
                                       []      -> return ()
                                       (n,_):_ -> setVar (p_finish p) n ]
    attachVar (p_finish p) (\v -> set finishC [ text := show v ])

    countC <- staticText cp [ text := showLocation 0,
                              fontSize := 16,
                              fontWeight := WeightBold ]
    attachVar (s_count s) (\v -> set countC [ text := showLocation v ])

    let doLayout = do
        horses <- getVar (s_horses s)
        dice <- getVar (s_dice s)
        set f [ layout := column 20
                   [ stretch $ horsesLayout horses,
                     hstretch $ diceLayout dice,
                     hstretch $ container cp $ 
                     margin 10 $ row 20 $ 
                     map widget buttons ++ [(widget finishC), 
                                            hglue,
                                            (widget countC)]
                   ]
                 ]
        where
            horsesLayout :: [Horse] -> Layout
            horsesLayout horses =
                if null horses then empty
                               else grid 10 10 $
                                    [ empty,
                                      hglue,
                                      space 50 0 ] :
                                    (map horseLayout horses)

            diceLayout :: [Die] -> Layout
            diceLayout dice =
                if null dice then empty
                             else row 10 $ map dieLayout dice

    attachVar (s_horses s) (\_ -> doLayout)
    attachVar (s_dice s) (\_ -> doLayout)

------------------------------------------------------------------------------
--
-- Actions
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--
-- Main Program
--
------------------------------------------------------------------------------



main :: IO ()
main = start ghr

ghr :: IO ()
ghr = do

    -- need the top level frame, since this is the parent of all our widgets
    f <- frame [text := "The Great Horse Race"]
    ticker <- timer f [ enabled := False ]

    --
    -- preferences
    --

    finishV <- mkVar 10
    durationV <- mkVar 10000
    rollerV <- mkVar (mkRoller 2 (replicate 6 1))

    let p = Pref finishV durationV rollerV 

    --
    -- state
    --

    horsesV <- mkVar []
    spareHorsesV <- mkVar []
    frontV <- mkVar 0
    countV <- mkVar 0

    diceV <- mkVar []
    spareDiceV <- mkVar []

    let s = State horsesV spareHorsesV
                  frontV countV diceV spareDiceV

    --
    -- actions
    --

    let configureHorses nReq nFirst = do
        configureWidgets (mkHorse f finishV frontV) horsesV spareHorsesV nReq
        -- number them
        horses <- getVar horsesV
        sequence_ $ map (\(h,i) -> numberHorse i h) (zip horses [nFirst ..])

    let configureDice = configureWidgets (mkDie f) diceV spareDiceV

        setDice xs = do configureDice (length xs)
                        dice <- getVar diceV
                        sequence_ $ map (\(d,x) -> setDie d x) (zip dice xs)

    let start d = do roller <- getVar rollerV
                     finish <- getVar finishV
                     let n = likelyRolls roller finish
                     set ticker [ interval := d `div` n,
                                  enabled := True ]

        stop = set ticker [ enabled := False ]

        restart = do stop
                     horses <- getVar horsesV
                     sequence_ $ map restartHorse horses
                     setVar frontV 0
                     setVar countV 0

        roll = do
            roller <- getVar rollerV
            horses <- getVar horsesV
            xs <- rollDice roller
            setDice xs
            updateVar countV (+1)
            sequence_ $ map (advanceIfHorse (sum xs)) horses

            -- check if race finished
            front <- getVar frontV
            finish <- getVar finishV
            if front >= finish then stop else return ()
            
    set ticker [ on command := roll ]

    let a = Actions roll start stop restart

    --
    -- actions in response to state updates
    -- 
    attachVar rollerV (\r -> configureHorses (nOutcomes r) (minOutcome r))

    roll
    restart

    createGui f p s a

    return ()