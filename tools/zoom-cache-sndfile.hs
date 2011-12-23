{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad (foldM)
import Control.Monad.State (execStateT)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as C
import Data.Default
import qualified Data.IntMap as IM
import qualified Data.Vector.Storable as SV
import Data.ZoomCache
import Data.ZoomCache.Dump
import Data.ZoomCache.Multichannel()
import Data.ZoomCache.PCM
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as SFV
import System.Console.GetOpt
import UI.Command

import Foreign

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , wmLevel  :: Int
    , track    :: TrackNo
    , intData  :: Bool
    , variable :: Bool
    , spec     :: TrackSpec
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , wmLevel  = 1024
    , track    = 1
    , intData  = False
    , variable = False
    , spec     = def { specDeltaEncode = False
                     , specZlibCompress = False
                     , specName = "gen"
                     }
    }

data Option = NoRaw
            | Watermark String
            | Track String
            | Delta
            | ZLib
            | Variable
            | IntData
            | Rate String
            | Label String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['w'] ["watermark"] (ReqArg Watermark "watermark")
             "Set high-watermark level"
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
    , Option ['d'] ["delta"] (NoArg Delta)
             "Delta-encode data"
    , Option ['Z'] ["zlib"] (NoArg ZLib)
             "Zlib-compress data"
    , Option ['b'] ["variable"] (NoArg Variable)
             "Generate variable-rate data"
    , Option ['i'] ["integer"] (NoArg IntData)
             "Generate ingeger data"
    , Option ['r'] ["rate"] (ReqArg Rate "data-rate")
             "Set track rate"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
    ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt RequireOrder options args of
        (opts, args', [] ) -> do
            config <- processConfig def opts
            return (config, args')
        (_,    _,     _:_) -> return (def, args)

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
    where
        processOneOption config NoRaw = do
            return $ config {noRaw = True}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}
        processOneOption config (Track s) = do
            return $ config {track = read s}

        processOneOption config Delta = do
            return $ config { spec = (spec config){specDeltaEncode = True} }
        processOneOption config ZLib = do
            return $ config { spec = (spec config){specZlibCompress = True} }
        processOneOption config Variable = do
            return $ config { variable = True
                            , spec = (spec config){specSRType = VariableSR}
                            }
        processOneOption config IntData = do
            return $ config { intData = True
                            , spec = setCodec (undefined :: Int) (spec config)
                            }
        processOneOption config (Rate s) = do
            return $ config { spec = (spec config){specRate = fromInteger $ read s} }
        processOneOption config (Label s) = do
            return $ config { spec = (spec config){specName = C.pack s} }

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zxd", "foo.zxd")]
        }

zoomGenHandler :: App () ()
zoomGenHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ zoomWriteFile config filenames

zoomWriteFile :: Config -> [FilePath] -> IO ()
zoomWriteFile _          []       = return ()
zoomWriteFile Config{..} (path:_)
    | intData   = w pcmInts path
    | otherwise = w pcmDoubles path
    where
    w :: (ZoomReadable a, ZoomWrite a, ZoomWrite (SampleOffset, a))
      => [a] -> FilePath -> IO ()
    w d
        | variable  = withFileWrite (IM.singleton track $ setCodec (head d) spec)
                          Nothing (not noRaw)
                          (sW >> mapM_ (write track) (zip (map SO [1,3..]) d))
        | otherwise = withFileWrite (IM.singleton track $ setCodec (head d) spec)
                          Nothing (not noRaw)
                          (sW >> mapM_ (write track) d)
    sW = setWatermark 1 wmLevel

------------------------------------------------------------

pcmDoubles :: [PCM Double]
pcmDoubles = map PCM doubles

pcmInts :: [PCM Int]
pcmInts = map PCM ints

doubles :: [Double]
doubles = take 10000000 $ map sin [0.0, 0.01 ..]

ints :: [Int]
ints = map (round . (* 32767.0)) doubles

------------------------------------------------------------

zoomInfo :: Command ()
zoomInfo = defCmd {
          cmdName = "info"
        , cmdHandler = zoomInfoHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Display basic info about a zoom-cache file"
        , cmdExamples = [("Display info about foo.zxd", "foo.zxd")]
        }

zoomInfoHandler :: App () ()
zoomInfoHandler = mapM_ (liftIO . zoomInfoFile pcmIdentifiers) =<< appArgs

------------------------------------------------------------

zoomDump :: Command ()
zoomDump = defCmd {
          cmdName = "dump"
        , cmdHandler = zoomDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom-cache data"
        , cmdExamples = [("Yo", "")]
        }

zoomDumpHandler :: App () ()
zoomDumpHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    mapM_ (liftIO . zoomDumpFile pcmIdentifiers (track config)) filenames

------------------------------------------------------------

zoomSummary :: Command ()
zoomSummary = defCmd {
          cmdName = "summary"
        , cmdHandler = zoomSummaryHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom-cache summary data"
        , cmdExamples = [("Read summary level 3 from foo.zxd", "3 foo.zxd")]
        }

zoomSummaryHandler :: App () ()
zoomSummaryHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO . (f (track config)) $ filenames
    where
        f trackNo (lvl:paths) = mapM_ (zoomDumpSummaryLevel (read lvl)
                                       pcmIdentifiers trackNo) paths
        f _ _ = putStrLn "Usage: zoom-cache summary n file.zxd"

------------------------------------------------------------

encode :: Command ()
encode = defCmd {
          cmdName = "encode"
        , cmdHandler = encodeHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Encode a zoom-cache-pcm file from sndfile data"
        , cmdExamples = [("Encode foo.wav", "foo.wav")]
        }

encodeHandler :: App () ()
encodeHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ mapM_ (encodeFile config) filenames

encodeFile :: Config -> FilePath -> IO ()
encodeFile Config{..} path = do
    h <- SF.openFile path SF.ReadMode info
    let sfInfo = SF.hInfo h
        sfRate = fromIntegral . SF.samplerate $ sfInfo
        sfChannels = fromIntegral . SF.channels $ sfInfo
        spec' = spec { specRate = sfRate }

    z <- openWrite (IM.singleton track $ setCodecMultichannel sfChannels (undefined :: PCM Double) spec')
             Nothing -- baseUTC
             True -- doRaw
             (path ++ ".zoom")
    z' <- foldFrames (encodeBuffer sfChannels) z h 1024
    closeWrite z'

    SF.hClose h
    where
        info = SF.Info 0 0 0 SF.defaultFormat 0 True

encodeBuffer :: Int -> ZoomWHandle -> SFV.Buffer Double -> IO ZoomWHandle
encodeBuffer channels z buf = execStateT (encV . SFV.fromBuffer $ buf) z
    where
        encV :: SV.Vector Double -> ZoomW ()
        encV v | SV.null v = return ()
               | otherwise = do
                   write 1 (map PCM . SV.toList . SV.take channels $ v)
                   encV (SV.drop channels v)

------------------------------------------------------------

sfDump :: Command ()
sfDump = defCmd {
          cmdName = "sfDump"
        , cmdHandler = sfDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read sndfile data"
        , cmdExamples = [("Read foo.wav", "foo.wav")]
        }

sfDumpHandler :: App () ()
sfDumpHandler = mapM_ (liftIO . sfDumpFile) =<< appArgs

sfDumpFile :: FilePath -> IO ()
sfDumpFile path = do
    h <- SF.openFile path SF.ReadMode info
    mapFrames_ dumpBuffer h 1024
    SF.hClose h
    where
        info = SF.Info 0 0 0 SF.defaultFormat 0 True

dumpBuffer :: SFV.Buffer Double -> IO ()
dumpBuffer = SV.mapM_ print . SFV.fromBuffer

mapFrames_ :: forall m a e . (MonadIO m, SF.Sample e, Storable e, SF.Buffer a e)
          => (a e -> m ()) -> SF.Handle -> SF.Count -> m ()
mapFrames_ f h n = do
    p <- liftIO $ mallocBytes (sizeOf (undefined :: e) * numChannels * n)
    fp <- liftIO $ newForeignPtr finalizerFree p
    v <- liftIO $ SF.fromForeignPtr fp 0 (n * numChannels)
    go p v
    where
       numChannels = SF.channels . SF.hInfo $ h
       go p v = do
           n' <- liftIO $ SF.hGetBuf h p n
           if n' == 0
               then return ()
               else do
                   f v
                   go p v

foldFrames :: forall m a e b . (MonadIO m, SF.Sample e, Storable e, SF.Buffer a e)
           => (b -> a e -> m b) -> b -> SF.Handle -> SF.Count -> m b
foldFrames f z0 h n = do
    p <- liftIO $ mallocBytes (sizeOf (undefined :: e) * numChannels * n)
    fp <- liftIO $ newForeignPtr finalizerFree p
    v <- liftIO $ SF.fromForeignPtr fp 0 (n * numChannels)
    go p v z0
    where
       numChannels = SF.channels . SF.hInfo $ h
       go p v z = do
           n' <- liftIO $ SF.hGetBuf h p n
           if n' == 0
               then return z
               else do
                   z' <- f z v
                   go p v z'

------------------------------------------------------------
-- The Application
--

zoom :: Application () ()
zoom = def {
          appName = "zoom-cache-sndfile"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial zoom-cache inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "Zoom"
        , appCmds = [ zoomGen
                    , zoomInfo
                    , zoomDump
                    , zoomSummary
                    , encode
                    , sfDump
                    ]
	}

longDesc :: String
longDesc = "Manipulate zoom-cache files"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
