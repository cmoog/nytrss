{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (join, when)
import Control.Monad.Catch (handleAll)
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.ByteString.UTF8 qualified as BS
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Database.SQLite.Simple qualified as SQL
import Network.HTTP.Simple qualified as HTTP
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    getCurrentDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hPrint, hPutStrLn, stderr)
import Text.HandsomeSoup qualified as HandsomeSoup
import Text.XML
  ( Document (..),
    Element (Element, elementAttributes, elementName, elementNodes),
    Name (Name, nameLocalName),
    Node (NodeContent, NodeElement),
    def,
    parseLBS,
    renderLBS,
  )
import Text.XML.HXT.Core qualified as HTX
import Text.XML.HXT.DOM.ShowXml (xshow)

{-
Every 20 minutes, iterate through each configured source feed.
  - Parse the source feed as RSS.
  - Iterate through each article link.
  - Fetch, parse, and extract the full article content.
  - Inject content node into original rss feed
  - Write to ./build.
  - After processing all feeds, upload ./build to R2 bucket.
-}

data Feed = Feed
  { fid :: String,
    fsource :: String
  }

feeds :: [Feed]
feeds =
  [ Feed
      { fid = "nyt/politics.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/Politics.xml"
      },
    Feed
      { fid = "nyt/business.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/Business.xml"
      },
    Feed
      { fid = "nyt/us.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/US.xml"
      },
    Feed
      { fid = "nyt/economy.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/Economy.xml"
      },
    Feed
      { fid = "nyt/sunday-review.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/sunday-review.xml"
      },
    Feed
      { fid = "nyt/home-page.rss",
        fsource = "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
      }
  ]

main :: IO ()
main = do
  hPutStrLn stderr "updating feeds..."
  handleAll (hPrint stderr) scrape
  hPutStrLn stderr "sleeping for 20 min..."
  threadDelay $ 20 * 60 * 1000 * 1000 -- 20 min
  main

scrape :: IO ()
scrape = do
  cwd <- getCurrentDirectory
  let buildDir = cwd </> "build"
  removeDirectoryRecursive buildDir
  createDirectory buildDir

  -- scrape each feed in series
  mapM_ (scrapeFeed buildDir) feeds

  let indexPath = buildDir </> "index.html"
  index <- generateIndex
  T.writeFile indexPath index

-- fetch feed source, fetch then extract article content
-- then, write new rss to file in build dir
scrapeFeed :: FilePath -> Feed -> IO ()
scrapeFeed buildDir Feed {fid, fsource} = do
  hPutStrLn stderr $ "processing " ++ fid
  req <- HTTP.parseRequest fsource
  resp <- HTTP.httpLBS req
  doc <- case parseLBS def $ HTTP.getResponseBody resp of
    Left e -> fail $ "xml parse error: " ++ show e
    Right a -> return a
  let emptyitems = articlesFromFeed doc
  newitems <- sequence $ mapMaybe (\a -> scrapeAndInject a <$> itemToUrl a) emptyitems

  let str = LBS.toString $ renderLBS def $ replaceItems doc newitems
  let filepath = buildDir </> fid

  createDirectoryIfMissing True $ takeDirectory filepath
  writeFile filepath str

scrapeAndInject :: Element -> String -> IO Element
scrapeAndInject element url = do
  conn <- SQL.open "cache.db"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS articles (url text PRIMARY KEY, html text);"
  cachedArticle <- readCache conn url
  content <- case cachedArticle of
    Just html -> return html
    Nothing -> do
      html <- fetchArticleContent url
      writeCache conn url html
      return html
  SQL.close conn
  let node = postContentNode $ T.pack content
  return $ consNode node element

-- | extract the article URL from the article <item> element
itemToUrl :: Element -> Maybe String
itemToUrl a =
  let linkNodes = takeChildren ("link" ==) a
      links = listToMaybe . mapMaybe maybeNodeContent . elementNodes <$> linkNodes
   in T.unpack <$> join (listToMaybe links)

consNode :: Node -> Element -> Element
consNode node (Element name attr nodes) = Element name attr (node : nodes)

-- | create a new <content> node with the given text as its child
postContentNode :: Text -> Node
postContentNode text =
  NodeElement
    ( Element
        { elementName = Name "content" Nothing Nothing,
          elementAttributes = mempty,
          elementNodes = [NodeContent text]
        }
    )

replaceItems :: Document -> [Element] -> Document
replaceItems doc@(Document pre (Element name attr _) epi) hydrated =
  let chan = channel doc
      newitems = NodeElement <$> hydrated
   in Document pre (Element name attr [NodeElement $ replaceChannelItems chan newitems]) epi

replaceChannelItems :: Element -> [Node] -> Element
replaceChannelItems elt newNodes =
  elt {elementNodes = (NodeElement <$> takeChildren ("item" /=) elt) ++ newNodes}

channel :: Document -> Element
channel = head . mapMaybe maybeNodeElement . elementNodes . documentRoot

articlesFromFeed :: Document -> [Element]
articlesFromFeed = takeChildren ("item" ==) . channel

takeChildren :: (Text -> Bool) -> Element -> [Element]
takeChildren f = filter (f . (nameLocalName . elementName)) . mapMaybe maybeNodeElement . elementNodes

maybeNodeContent :: Node -> Maybe Text
maybeNodeContent (NodeContent a) = Just a
maybeNodeContent _ = Nothing

maybeNodeElement :: Node -> Maybe Element
maybeNodeElement (NodeElement a) = Just a
maybeNodeElement _ = Nothing

-- generate index.html with injected timestamp
generateIndex :: IO Text
generateIndex = do
  template <- T.readFile "./index.html"
  time <- showTime
  return $ T.replace "{{LastUpdated}}" (T.pack time) template

-- | show a timestamp in the current timezone
showTime :: IO String
showTime = do
  zone <- getCurrentTimeZone
  utc <- getCurrentTime
  return $ show (utcToLocalTime zone utc) <> " " <> show zone

-- extract article content from raw full article html
fetchArticleContent :: String -> IO String
fetchArticleContent url = do
  threadDelay $ 1 * 1000 * 1000 -- 1 second
  req <- HTTP.parseRequest url
  resp <- HTTP.httpBS $ HTTP.setRequestHeaders headers req
  let statusCode = HTTP.getResponseStatusCode resp
  when (statusCode /= 200) $ fail $ "request failed: " ++ show statusCode
  let doc = HandsomeSoup.parseHtml $ BS.toString $ HTTP.getResponseBody resp
  content <- HTX.runX $ (HTX.>>>) doc $ HandsomeSoup.css "section[name='articleBody']"
  return $ xshow content

headers :: [HTTP.Header]
headers =
  [ ("Sec-Ch-Ua-Mobile", "?0"),
    ("Sec-Fetch-Dest", "image"),
    ("Sec-Fetch-Mode", "no-cors"),
    ("Sec-Fetch-Site", "cross-site"),
    ("Cache-Control", "no-cache"),
    ("Sec-Ch-Ua-Platform", "\"macOS\""),
    ("Sec-Ch-Ua", "\"Chromium\";v=\"118\", \"Google Chrome\";v=\"118\", \"Not=A?Brand\";v=\"99\""),
    ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36")
  ]

newtype Article = Article {html :: String}

instance SQL.FromRow Article where
  fromRow = Article <$> SQL.field

writeCache :: SQL.Connection -> String -> String -> IO ()
writeCache conn url html =
  SQL.execute conn "INSERT INTO articles (url, html) VALUES (?, ?)" (url, html)

readCache :: SQL.Connection -> String -> IO (Maybe String)
readCache conn url = do
  results <- SQL.query conn "SELECT html FROM articles WHERE url = ?" $ SQL.Only url
  return $ html <$> listToMaybe results
