import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State (MonadIO, evalStateT)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Array (Array, listArray, (!))
import Data.Binary (Word8)
import Data.Char (ord)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)
import System.IO

data BfState = BfState
    { mem :: IntMap.IntMap Word8
    , loop :: Int
    , srcIx :: Int
    , memIx :: Int
    }

data BfError = OutOfBoundsError
    deriving (Show)

type BfEval = (StateT BfState (ExceptT BfError IO)) ()

type Source = Array Int Char

forward :: Source -> BfEval
forward src = do
    st <- get
    let l = loop st
        sIx = srcIx st
    when (l > 0) $ do
        if sIx == length src
            then throwError OutOfBoundsError
            else case src ! sIx of
                '[' -> put st{loop = l + 1, srcIx = sIx + 1} >> forward src
                ']' ->
                    if l == 1
                        then put st{loop = 0}
                        else put st{loop = l - 1, srcIx = sIx + 1} >> forward src
                _ -> put st{srcIx = sIx + 1} >> forward src

backward :: Source -> BfEval
backward src = do
    st <- get
    let l = loop st
        sIx = srcIx st
    when (l > 0) $ do
        if sIx < 0
            then throwError OutOfBoundsError
            else case src ! sIx of
                '[' ->
                    if l == 1
                        then put st{loop = 0}
                        else put st{loop = l - 1, srcIx = sIx - 1} >> backward src
                ']' -> put st{loop = l + 1, srcIx = sIx - 1} >> backward src
                _ -> put st{srcIx = sIx - 1} >> backward src

brainf :: Source -> BfEval
brainf src = do
    st <- get
    let m = mem st
        mIx = memIx st
        sIx = srcIx st
        v = IntMap.findWithDefault 0 mIx m
    if sIx == length src
        then liftIO $ putStr "\n"
        else do
            case src ! sIx of
                '+' -> do
                    put st{mem = IntMap.insert mIx (v + 1) m, srcIx = sIx + 1}
                '-' -> do
                    put st{mem = IntMap.insert mIx (v - 1) m, srcIx = sIx + 1}
                '>' -> do
                    put st{memIx = (mIx + 1) `rem` 30000, srcIx = sIx + 1}
                '<' -> do
                    put st{memIx = if mIx == 0 then 29999 else mIx - 1, srcIx = sIx + 1}
                '[' -> do
                    if v == 0
                        then put st{loop = 1, srcIx = sIx + 1} >> forward src
                        else put st{srcIx = sIx + 1}
                ']' -> do
                    if v /= 0
                        then put st{loop = 1, srcIx = sIx - 1} >> backward src
                        else put st{srcIx = sIx + 1}
                ',' -> do
                    c <- liftIO getChar
                    put st{mem = IntMap.insert mIx ((toEnum . fromEnum) c) m, srcIx = sIx + 1}
                '.' -> do
                    liftIO $ putChar $ (toEnum . fromEnum) v
                    liftIO $ hFlush stdout
                    put st{srcIx = sIx + 1}
                _ -> put st{srcIx = sIx + 1}
            brainf src

initState :: BfState
initState =
    BfState
        { mem = IntMap.empty
        , srcIx = 0
        , memIx = 0
        , loop = 0
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> do
            file <- readFile fname
            let src = listArray (0, length file - 1) file
            res <- runExceptT (runStateT (brainf src) (initState))
            case res of
                Left e -> print e
                Right _ -> return ()
        _ -> print "usage: ./brainf <filepath>"
