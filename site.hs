--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import           Data.Char
import           Data.List
import           Data.Either
import           Data.Maybe
import qualified Data.Map as M
import           Data.Monoid (mappend)
import           Data.String
import           Hakyll
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import qualified Data.ByteString.Lazy.Char8 as C
import           Debug.Trace
import           Text.Jasmine
import           Graphics.HsExif
import           System.Process
import           System.FilePath.Posix

--------------------------------------------------------------------------------
-- | Javascript compiler as found on
-- | codetalk.io/posts/2016-05-10-compiling-scss-and-js-in-hakyll.html
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

applyPandoc :: ([Block] -> [Block]) -> Pandoc -> Pandoc
applyPandoc f (Pandoc h bs) = Pandoc h (f bs)

liftImages :: Block -> [Block]
liftImages (Para (i@(Image _ _ _):ls)) = (Div ("image", [], []) [Plain [i]]) : (liftImages (Para ls))
liftImages (Para [])                   = []
liftImages (Para ls)                   = Para ls' : liftImages (Para ls'')
  where isImage (Image _ _ _) = True
        isImage _             = False
        ls'                   = takeWhile (not . isImage) ls
        ls''                  = dropWhile (not . isImage) ls
liftImages x = [x]

transformHeaders :: [Block] -> [Block]
transformHeaders ((Header l _ ((Str s):_)):bs) = (Div (s', [], []) bs') : transformHeaders bs''
  where s'   = map toLower (take 2  s)
        bs'  = takeWhile (not . isDivOrHeader) bs
        bs'' = dropWhile (not . isDivOrHeader) bs
        isDivOrHeader (Header l _ _)    = l == 1
        isDivOrHeader (Div (s'', _, _) _) = s'' == "image"
        isDivOrHeader _               = False
transformHeaders (x:xs) = x : transformHeaders xs
transformHeaders     [] = []

groupDivs :: [Block] -> [Block]
groupDivs (d@(Div (s, _, _) _):bs)
  | s == "image" = d : groupDivs bs
  | otherwise    = (Div ("float", [], []) (d:bs')) : (groupDivs bs'')
  where bs'  = takeWhile (not . isSameHeaderOrImage) bs
        bs'' = dropWhile (not . isSameHeaderOrImage) bs
        isSameHeaderOrImage (Div (s', _, _) _) = (s == s') || (s' == "image")
        isSameHeaderOrImage _                  = False
groupDivs (x:xs) = x : groupDivs xs
groupDivs []     = []

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  pandoc <- getResourceBody >>= readPandoc
  return $ writePandoc ((traceShowId (applyPandoc (groupDivs . transformHeaders . (concatMap liftImages)) <$> pandoc)))

extractImagePathAndPos :: Inline -> String
extractImagePathAndPos (Image _ _ t) = ",[\"" ++ fst t ++ "\"," ++ pos ++ "]"
  where pos = (traceShowId (dropWhile (\x -> elem x ("fig:" :: [Char])) (snd t)))
extractImagePathAndPos _             = []

mInvert :: Monad a => [a b] -> a [b]
mInvert [] = return []
mInvert (a:aa) =  do
  bb <- mInvert aa
  b  <- a
  return (b:bb)

gpsPosCompiler :: Compiler (Item String)
gpsPosCompiler = do
  posts   <- recentFirst =<< loadAllSnapshots "posts/*" "raw_content"
  let pandocs = mInvert $ map readPandoc posts
  imgs    <-  map ((drop 1) . traceShowId . (query extractImagePathAndPos) . itemBody) <$> pandocs
  names   <- (map (toFilePath . itemIdentifier)) <$> pandocs
  makeItem $ "var images = [\n"
             ++ unlines (intersperse "," (zipWith toJsList names imgs))
             ++ "]"
    where toJsList x y = "[\"" ++ x ++ "\", [" ++ y ++ "]]"

thumbnailCompiler :: Compiler ()
thumbnailCompiler = do
  path <- getResourceFilePath
  iden <- getUnderlying
  trgt <- getRoute iden
  unsafeCompiler (callCommand (traceShowId ("exiftool -b -ThumbnailImage "
                                            ++ path ++ " > "
                                            ++ fromJust trgt ++ ".jpg")))

main :: IO ()
main = hakyll $ do
    match "images/*.JPG" $ do
        route   idRoute
        compile $ do
          thumbnailCompiler
          copyFileCompiler

    match "images/*.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*.png" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
      route   idRoute
      compile compressJsCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ getResourceBody
        >>= saveSnapshot "raw_content"
        >>  customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

--     match "posts/*" $ do
--       route $ gsubRoute "^posts/" (const "gps_tracks/")

    create ["images.js"] $ do
      route idRoute
      compile $ do
        gpsPosCompiler


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return (take 1 posts)) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  bodyField     "body"     `mappend`
  field "title" (\x -> do
    let identifier = itemIdentifier x
    week <- getMetadataField' identifier "week"
    return (traceShowId ("<span id=\"h_en\">Week " ++ show (round (read week)) ++ " /</span>"
                         ++ "<span id=\"h_de\"> Woche " ++ show (round (read week)) ++ "</span>"))) `mappend`
  metadataField            `mappend`
  urlField      "url"      `mappend`
  pathField     "path"     `mappend`
  missingField
