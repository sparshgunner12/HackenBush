import Data.HashMap as M
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import System.Random

type HM = M.Map (Integer,Integer) Integer
type BranchObject = (Integer, BranchColor)
type Board = [Bush BranchObject]
data BranchColor = Red | Green | Blue | White deriving (Read, Show, Enum, Eq, Ord)
data Bush a = Empty | Branch a [Bush a] deriving (Read, Show, Eq, Ord)
data GameType = Single | Multiple deriving (Read, Show, Eq, Ord)

data GameState = GameState { board :: Board, playerNames :: [String] , activePlayer :: Int, mp :: HM, gameType :: GameType , randomGen :: Int}
data GUI = GUI {
    getPlayBoard :: IO DrawingArea,
    drawWindow :: IO DrawWindow,
    disableBoard :: IO (),
    enableBoard :: IO (),
    resetBoard :: IO (),
    showPlayDialog :: IO (),
    getRadioButtonValue :: Int -> IO Bool,
    showPlayerDialog :: GameType -> IO (),
    getPlayerNames :: GameType -> IO [String],
    showDifficultyDialog :: IO (),
    getDifficulty :: IO Int,
    hideDialog :: Int -> IO (),
    setStatus :: String -> IO ()
}

data RGB = RGB {
    r :: Double,
    g :: Double,
    b :: Double
}

branchLength = -60
boardWidth = 1160 :: Double
boardHeight = 600 :: Double

main :: IO ()
main = do
    initGUI

    g <- newStdGen

    Just xml <- xmlNew "haskenbush.glade"
    window <- xmlGetWidget xml castToWindow "mWindow"
    window `onDestroy` mainQuit

    newgame <- xmlGetWidget xml castToMenuItem "newGame"
    quit <- xmlGetWidget xml castToMenuItem "quit"
    help <- xmlGetWidget xml castToMenuItem "about"

    gameBoard <- xmlGetWidget xml castToDrawingArea "gameBoard"
    statusBar <- xmlGetWidget xml castToStatusbar "gameStatus"
    buttons <- mapM (xmlGetWidget xml castToButton) ["bPlayOk", "bPlayerOk", "bDiffOk"]
    radioButtons <- mapM (xmlGetWidget xml castToRadioButton) ["rbSingle", "rbMultiple"]
    dialogs <- mapM (xmlGetWidget xml castToDialog) ["playDialog", "playerDialog", "difficultyDialog"]
    playerNameEntry <- mapM (xmlGetWidget xml castToEntry) ["player1Entry","player2Entry"]
    aboutDialog <- xmlGetWidget xml castToAboutDialog "aboutDialog"
    cBox <- xmlGetWidget xml castToComboBox "cbDifficulty"
    ctx <- statusbarGetContextId statusBar "state"

    gui <- guiActions buttons radioButtons dialogs cBox playerNameEntry gameBoard statusBar ctx

    widgetModifyBg gameBoard StateNormal (Color 65535 65535 65535)

    widgetShowAll window

    state <- newIORef GameState { board = [Empty], playerNames = ["Player 1", "Player 2"], activePlayer = 0, mp = M.empty :: HM, gameType = Multiple, randomGen = 0}
    let modifyState f = readIORef state >>= f >>= writeIORef state
    reset gui
    onActivateLeaf quit mainQuit
    onActivateLeaf newgame (reset gui)
    onActivateLeaf help (widgetShow aboutDialog)

    onButtonPress (buttons!!0) (\e -> do
                                    modifyState $ updateGameType gui
                                    return True
                                    )

    onButtonPress (buttons!!1) (\e -> do
                                    modifyState $ updatePlayerNames gui
                                    return True
                                    )

    onButtonPress (buttons!!2) (\e -> do
                                    modifyState $ updateDifficulty gui
                                    return True
                                    )

    onButtonPress gameBoard (\e -> do
                                    let x = truncate (eventX e)
                                    let y = truncate (eventY e)
                                    modifyState $ removeBranch gui x y
                                    return True
                                    )

    onExpose gameBoard (\x -> do
                                modifyState $ drawCanvas gameBoard
                                return True
                                )

    mainGUI

guiActions buttons radioButtons dialogs cBox playerNameEntry gameBoard statusBar ctx =
    do return $ GUI {disableBoard = flip widgetSetSensitivity False gameBoard,
                     enableBoard = flip widgetSetSensitivity True gameBoard,
                     resetBoard = do
                        drawWin <- widgetGetDrawWindow gameBoard
                        drawWindowClear drawWin
                        flip widgetSetSensitivity True gameBoard,
                     getPlayBoard = do return gameBoard,
                     drawWindow = do
                        drawWin <- widgetGetDrawWindow gameBoard
                        return drawWin,
                     showPlayDialog = widgetShow (dialogs!!0),
                     showDifficultyDialog = do
                                comboBoxSetActive cBox 0
                                widgetShow (dialogs!!2),
                     showPlayerDialog = \gameType -> do
                                entrySetText (playerNameEntry!!1) "Player 1"
                                if gameType == Single 
                                    then do
                                        entrySetText (playerNameEntry!!1) "Jarvis"
                                        set (playerNameEntry!!1) [entryEditable := False]
                                    else do
                                        entrySetText (playerNameEntry!!1) "Player 2"
                                        set (playerNameEntry!!1) [entryEditable := False]
                                widgetShow (dialogs!!1),
                     hideDialog = \i -> do
                        widgetHide (dialogs!!i),
                     getDifficulty = comboBoxGetActive cBox,
                     getPlayerNames = \gameType -> mapM (entryGetText) playerNameEntry,
                     getRadioButtonValue = \i ->  (toggleButtonGetActive (radioButtons!!i)),
                     setStatus = \msg -> do
                                 statusbarPop statusBar ctx
                                 statusbarPush statusBar ctx msg
                                 return ()}

reset :: GUI -> IO ()
reset gui = do
    resetBoard gui
    setStatus gui ""
    disableBoard gui
    showPlayDialog gui

updateGameType :: GUI -> GameState -> IO GameState
updateGameType gui st@(GameState board playerNames activePlayer mp gameType randomGen) = do
    isSingle <- getRadioButtonValue gui 0
    hideDialog gui 0
    let newGameType = if isSingle then Single else Multiple
    showPlayerDialog gui newGameType
    return (GameState board playerNames 0 mp newGameType randomGen)

updatePlayerNames :: GUI -> GameState -> IO GameState
updatePlayerNames gui st@(GameState board playerNames activePlayer mp gameType randomGen) = do
    newPlayerNames <- getPlayerNames gui Single
    hideDialog gui 1
    showDifficultyDialog gui
    return (GameState board newPlayerNames activePlayer mp gameType randomGen)

updateDifficulty :: GUI -> GameState -> IO GameState
updateDifficulty gui st@(GameState board playerNames activePlayer mp gameType randomGen) = do
    g <- newStdGen
    selected <- getDifficulty gui
    hideDialog gui 2
    let difficulty = 1 + toInteger selected
    let randTuple = randomNumber g (1 + difficulty) (2 + (4*difficulty + difficulty+1) `div` 3)
    let numberOfBushes = fst randTuple
    let g' = snd randTuple
    let newBoard = randomBoard numberOfBushes difficulty 0 g'
    drawingBoard <- drawWindow gui
    drawWindowClearAreaExpose drawingBoard 0 0 (truncate (boardWidth+50)) (truncate (boardHeight+50))
    let s = " : Cut a " ++ show (getColor activePlayer)++ " / Green branch." :: String
    setStatus gui $ playerNames!!0 ++ s
    enableBoard gui
    return (GameState newBoard playerNames activePlayer mp gameType randomGen)

removeBranch :: GUI -> Integer -> Integer -> GameState -> IO GameState
removeBranch gui x y st@(GameState board playerName activePlayer mp gameType randomGen)= do
    let id = getId mp x y
    if id /= -1 
        then do let color = findInBushList board id
                if (isValidColor color activePlayer)
                    then do let newBoard = cutBush board id
                            let nextPlayer = mod ( activePlayer + 1 ) 2
                            drawingBoard <- drawWindow gui
                            drawWindowClearAreaExpose drawingBoard 0 0 (truncate (boardWidth+50)) (truncate (boardHeight+50))
                            if wins newBoard $ getColor nextPlayer 
                                then do let s = " : Wins." :: String
                                        setStatus gui $ playerName!!(mod (nextPlayer+1) 2) ++ s
                                        disableBoard gui
                                        return (GameState newBoard playerName nextPlayer mp gameType randomGen)
                                else do let s = " : Cut a " ++ show (getColor nextPlayer)++ " / Green branch." :: String
                                        setStatus gui $ playerName!!nextPlayer ++ s
                                        if gameType == Single then do
                                            let id = optimalplay (getColor nextPlayer) newBoard (randomGen+1)
                                            let newBoard' = cutBush newBoard id
                                            drawingBoard <- drawWindow gui
                                            drawWindowClearAreaExpose drawingBoard 0 0 (truncate (boardWidth+50)) (truncate (boardHeight+50))
                                            if wins newBoard' $ getColor activePlayer
                                                then do let s' = " : Wins." :: String
                                                        setStatus gui $ playerName!!nextPlayer ++ s'
                                                        disableBoard gui
                                                else do
                                                    let s' = " : Cut a " ++ show (getColor activePlayer)++ " / Green branch." :: String
                                                    setStatus gui $ playerName!!activePlayer ++ s'
                                            return (GameState newBoard' playerName activePlayer mp gameType (randomGen+1))
                                        else do
                                            return (GameState newBoard playerName nextPlayer mp gameType randomGen)
                            
                    else do 
                        return st
        else do
            return st

wins :: Board -> BranchColor -> Bool
wins board color 
    | findColorInBushList board color = False
    | findColorInBushList board Green = False
    | otherwise = True

getId :: HM -> Integer -> Integer -> Integer
getId mp x y
    | member (x,y) mp = fromJust $ M.lookup (x,y) mp
    | member (x,y+1) mp = fromJust $ M.lookup (x,y+1) mp
    | member (x,y-1) mp = fromJust $ M.lookup (x,y-1) mp
    | member (x+1,y) mp = fromJust $ M.lookup (x+1,y) mp
    | member (x+1,y+1) mp = fromJust $ M.lookup (x+1,y+1) mp
    | member (x+1,y-1) mp = fromJust $  M.lookup (x+1,y-1) mp
    | member (x-1,y) mp = fromJust $ M.lookup (x-1,y) mp
    | member (x-1,y+1) mp = fromJust $ M.lookup (x-1,y+1) mp
    | member (x-1,y-1) mp = fromJust $ M.lookup (x-1,y-1) mp
    | otherwise = -1

getColor :: (Num a, Eq a) => a -> BranchColor
getColor id
    | id == 0 = Red
    | id == 1 = Blue
    | otherwise = White

isValidColor :: (Num a, Eq a) => BranchColor -> a -> Bool
isValidColor color id
    | id == 0 && color == Red = True
    | id == 1 && color == Blue = True
    | color == Green = True
    | otherwise = False

drawCanvas :: DrawingArea -> GameState -> IO GameState
drawCanvas gameBoard st@(GameState board playerName activePlayer mp gameType randomGen) = do
    let numberOfBushes = fromIntegral $ (length board)
    let divLength =  boardWidth / numberOfBushes
    let startPointX = 50 + divLength/2
    let startPointY = boardHeight - 75
    mp'<-drawBoard board mp gameBoard startPointX startPointY divLength
    drawBottomLine gameBoard
    return (GameState board playerName activePlayer mp' gameType randomGen)

drawBoard :: Board -> HM -> DrawingArea -> Double -> Double -> Double -> IO HM
drawBoard [] mp _ _ _ _= do return mp 
drawBoard (bush:board) mp canvas startPointX startPointY divLength= do
    mp' <- drawBush bush mp canvas startPointX startPointY 90
    mp'' <- drawBoard board mp' canvas (startPointX + divLength) startPointY divLength
    return mp''

drawBush :: Bush BranchObject -> HM -> DrawingArea -> Double -> Double -> Double -> IO HM
drawBush (Branch branch bushes) mp canvas startPointX startPointY angle = do
    let endPoints = getEndPoints startPointX startPointY angle branchLength
    let endPointX = fst endPoints
    let endPointY = snd endPoints
    mp' <- drawBranch branch mp canvas startPointX startPointY endPointX endPointY
    let numberOfSubBushes = fromIntegral $ length bushes
    if numberOfSubBushes > 0 
        then do let angleDiff = 180.0 / numberOfSubBushes
                let startAngle = angle - 90.0 + angleDiff / 2.0
                mp'' <- drawBushList bushes mp' canvas endPointX endPointY startAngle angleDiff
                return mp''
        else do mp'' <- drawBushList [] mp' canvas 0 0 0 0
                return mp''
drawBush Empty mp _ _ _ _ = do return mp

drawBushList :: Board -> HM -> DrawingArea -> Double -> Double -> Double -> Double -> IO HM
drawBushList [] mp _ _ _ _ _ = do return mp
drawBushList (bush:bushes) mp canvas startPointX startPointY angle angleDiff= do
    mp' <- drawBush bush mp canvas startPointX startPointY angle
    mp''<- drawBushList bushes mp' canvas startPointX startPointY (angle+angleDiff) angleDiff
    return mp''

drawBranch :: BranchObject -> HM -> DrawingArea -> Double -> Double -> Double -> Double -> IO HM
drawBranch branch mp canvas startPointX startPointY endPointX endPointY= do
    let id = fst branch
    let color = getRGB $ snd branch
    drawWin <- widgetGetDrawWindow canvas
    renderWithDrawable drawWin $ do
        setSourceRGB (r color) (g color) (b color)
        setLineWidth 6
        setLineCap LineCapRound
        setLineJoin LineJoinMiter

        moveTo startPointX startPointY
        lineTo endPointX endPointY
        stroke

    let mp' = updateMapMultiple 7 startPointX startPointY endPointX endPointY id mp
    return mp'

drawBottomLine :: DrawingArea -> IO ()
drawBottomLine canvas = do
    drawWin <- widgetGetDrawWindow canvas
    renderWithDrawable drawWin $ do
        setSourceRGB 0.12 0.25 0.12
        setLineWidth 6
        setLineCap LineCapRound
        setLineJoin LineJoinMiter

        moveTo 50 $ boardHeight - 70
        lineTo boardWidth $boardHeight - 70
        stroke

getEndPoints :: Double -> Double -> Double -> Double -> (Double, Double)
getEndPoints x y angle length = (x2, y2) where
    radians = getRadians angle
    x2 = x + length * (cos radians)
    y2 = y + length * (sin radians)

updateMapMultiple :: Double -> Double -> Double -> Double -> Double -> Integer -> HM -> HM
updateMapMultiple t x1 y1 x2 y2 id mp
    | t < 0 = mp
    | otherwise = updateMapMultiple (t-1) x1 y1 x2 y2 id $ updateMap (x1+t) y1 (x2+t) y2 id mp

updateMap :: Double -> Double -> Double -> Double -> Integer -> HM -> HM
updateMap x1 y1 x2 y2 id mp =
    let yDiff = y2-y1 
        xDiff = x2-x1
        points =  if (abs yDiff) <= (abs xDiff) 
                      then let m = (y2 - y1) / (x2-x1)
                      in if (x2-x1) >= 0 then [ ((x1 + x, y1 + x*m),id) | x <- [ (x2 -x1), (x2 - x1 - 1) .. 0] ]
                         else  [ ((x2 + x, y2 + x*m),id) | x <- [ (x1 -x2), (x1 - x2 - 1) .. 0] ]
                  else let m = (x2 - x1) / (y2-y1)
                      in if (y2-y1) >= 0 then [ ((x1 + m*y, y1 + y),id) | y <- [ (y2 - y1), (y2 - y1 - 1) .. 0] ]
                         else [ ((x2 + m*y, y2 + y),id) | y <- [ (y1 - y2), (y1 - y2 - 1) .. 0] ]
    in insertIntoMap mp points

insertIntoMap :: HM -> [((Double, Double) ,Integer)] -> HM
insertIntoMap mp [] = mp
insertIntoMap mp (idPoint:idPoints) =
    let point = fst idPoint
        rndX = round $ fst point
        rndY = round $ snd point
        rndP = (rndX, rndY)
    in M.insert rndP (snd idPoint) $ insertIntoMap mp idPoints

getRGB :: BranchColor -> RGB
getRGB branchColor
    | branchColor == Red = RGB 1 0 0
    | branchColor == Green = RGB 0 1 0
    | branchColor == Blue = RGB 0 0 1
    | branchColor == White = RGB 1 1 1

getRadians :: Double -> Double
getRadians degrees = radians where
    radians = (degrees * pi) / 180

randomColor :: RandomGen g => g -> (BranchColor, g)
randomColor g = case randomR (0,2) g of (r, g') -> (toEnum r, g')

randomNumber :: RandomGen g => g -> Integer -> Integer -> (Integer, g)
randomNumber g st en = do
    randomR (st::Integer, en::Integer) g

randomBushTuple :: (RandomGen g, Ord a, Num a) => a -> Integer -> Integer -> g -> (Bush BranchObject, g)
randomBushTuple height level count g
    | height <= 0 = (Empty, g)
    | otherwise = let randColorTuple = randomColor g
                      branchObject = ( count , fst randColorTuple )
                      g' = snd randColorTuple
                      randTuple = randomNumber g' 0 (2*level-1)
                      numberOfSubBushes = fst randTuple
                      g'' = snd randTuple
                      bush = Branch branchObject $ randomBushList numberOfSubBushes (height-1) level (count+1) g''
                  in (bush, g'')

randomBushList :: (Num a, Ord a, RandomGen g) => Integer -> a -> Integer -> Integer -> g -> Board
randomBushList 0 _ _ _ _ = []
randomBushList numberOfSubBushes height level count g= 
    let bushTuple = randomBushTuple (height-1) level count g
        bush = fst bushTuple
        len = (findMaxId bush) + 1
        g' = snd bushTuple
    in bush : randomBushList (numberOfSubBushes-1) height level (count+len) g'

randomBoard :: (Num a, Ord a, RandomGen g) => a -> Integer -> Integer -> g -> Board
randomBoard numberOfBushes level count g
    | numberOfBushes<=0 = []
    | otherwise = let randTuple = randomNumber g 2 (2*level)
                      height = fst randTuple
                      g' = snd randTuple
                      bushTuple = randomBushTuple height level count g'
                      bush = fst bushTuple
                      len = (findMaxId bush) + 1
                      g'' = snd bushTuple
                  in bush : randomBoard (numberOfBushes-1) level (count+len) g''

flattenBushList :: Board -> [BranchObject]
flattenBushList [] = []
flattenBushList (bush:bushes) = (flattenBush bush) ++ (flattenBushList bushes)

flattenBush :: Bush BranchObject -> [BranchObject]
flattenBush Empty = []
flattenBush (Branch a bushes)= a : (flattenBushList bushes)

countNodes :: Board -> Int
countNodes [] = 0
countNodes board = length $ flattenBushList board

findMaxIdList :: Board -> Integer
findMaxIdList [] = 0
findMaxIdList (bush:bushes) = max (findMaxId bush) (findMaxIdList bushes)

findMaxId :: Bush BranchObject -> Integer
findMaxId Empty = -1
findMaxId (Branch a bushes)= max (fst a) (findMaxIdList bushes)

findInBushList :: Board -> Integer -> BranchColor
findInBushList [] _ = White
findInBushList (bush:bushes) id = if (findInBush bush id) /= White then (findInBush bush id) else findInBushList bushes id

findInBush :: Bush BranchObject -> Integer -> BranchColor
findInBush Empty _ = White
findInBush (Branch branch bushes) id = if id == (fst branch) then (snd branch) else findInBushList bushes id

findColorInBushList :: Board -> BranchColor -> Bool
findColorInBushList [] _ = False
findColorInBushList (bush:bushes) color = if (findColorInBush bush color) then True else findColorInBushList bushes color

findColorInBush :: Bush BranchObject -> BranchColor -> Bool
findColorInBush Empty _ = False
findColorInBush (Branch branch bushes) color = if color == (snd branch) then True else findColorInBushList bushes color

removeBushList :: Board -> Integer -> Board
removeBushList [] id = []
removeBushList (bush:bushes) id = removeBush bush id : removeBushList bushes id

removeBush :: Bush BranchObject -> Integer -> Bush BranchObject
removeBush Empty _ = Empty
removeBush (Branch branch bushes) id = if id==fst branch then Empty else Branch branch $removeBushList bushes id

cutBush :: Board -> Integer -> Board
cutBush [] _ = []
cutBush (bush:bushes) id = if(findInBush bush id) /= White
                            then (removeBush bush id) : bushes
                            else bush : cutBush bushes id

findFirstIdBushList turn []=(-1)
findFirstIdBushList turn (bush:bushrest)=if(findFirstIdBush  turn bush/=(-1))then findFirstIdBush turn bush else  findFirstIdBushList turn bushrest
findFirstIdBush turn Empty=(-1)
findFirstIdBush turn (Branch b bushes)=if(snd b==turn || snd b==Green)then fst b else findFirstIdBushList turn bushes

findlistBush turn Empty=[]
findlistBush turn (Branch b bushes)=if(snd b==turn || snd b==Green)then [fst b] ++ find_list turn bushes else find_list turn bushes

find_list turn []= []
find_list turn (bush:bushrest)=findlistBush turn bush ++ find_list turn bushrest

play_random_id turn [] _= (-1)
play_random_id turn bush randomGen = let list = find_list turn bush
                                     in list !! fromIntegral (randomGen `mod` length list)

extractIdBushes _ []=[]
extractIdBushes turn (bush:bushrest)=(extract_id_bush turn bush++extractIdBushes turn bushrest) 
extract_id_bush turn Empty=[]
extract_id_bush turn (Branch b bushes)=if(snd b==turn || snd b==Green)then ([fst b]++extractIdBushes turn bushes) else (extractIdBushes turn bushes)
extractId _ []=[]
extractId turn (bush:bushrest)=extract_id_bush turn bush++extractId turn bushrest
isEmpty_bush Empty=True
isEmpty_bush (Branch b bushes)=False
isEmpty []=True
isEmpty (bush:bushrest)=if(isEmpty_bush bush)then isEmpty bushrest else False

findWinningPosition Blue [] _=(-1)
findWinningPosition Red [] _=(-1)
findWinningPosition Blue (x:xs) board=if(isWinningPosition Red (cutBush board x)==(-1))then x else findWinningPosition Blue xs board
findWinningPosition Red (x:xs) board=if(isWinningPosition Blue (cutBush board x)==(-1))then x else findWinningPosition Red xs board

isWinningPosition Blue board=if(isEmpty board)then (-1) else findWinningPosition Blue (extractId Blue board) board
isWinningPosition Red board=if(isEmpty board)then (-1) else findWinningPosition Red (extractId Red board) board
optimalpla turn board randomGen =if(isWinningPosition turn board /=(-1))then isWinningPosition turn board else play_random_id turn board randomGen

optimalplay turn board randomGen =if(countNodes board<=20)then optimalpla turn board randomGen else play_random_id turn board randomGen