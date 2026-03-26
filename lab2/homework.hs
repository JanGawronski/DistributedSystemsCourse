{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Media ((//))
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import Data.Map (Map)
import Language.Haskell.TH (isInstance)
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.Algorithm.Diff
import Data.Either (Either (Right))
import Data.List (concat)
import qualified Data.List as L
import Lucid
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.Swagger (Swagger, ToSchema(..), defaultSchemaOptions, genericDeclareNamedSchemaUnrestricted, NamedSchema(..))
import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_)

data Translation = Translation
  { id :: String
  , name :: String
  , englishName :: String
  , website :: String
  , licenseUrl :: String
  , shortName :: String
  , language :: String
  , languageName :: Maybe String
  , languageEnglishName :: Maybe String
  , textDirection :: String
  , availableFormats :: [String]
  , listOfBooksApiLink :: String
  , numberOfBooks :: Int
  , totalNumberOfChapters :: Int
  , totalNumberOfVerses :: Int
  , numberOfApocryphalBooks :: Maybe Int
  , totalNumberOfApocryphalChapters :: Maybe Int
  , totalNumberOfApocryphalVerses :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON Translation
instance ToJSON Translation
instance ToSchema Translation

data TranslationsResponse = TranslationsResponse
  { translations :: [Translation]
  } deriving (Show, Eq, Generic)

instance FromJSON TranslationsResponse
instance ToJSON TranslationsResponse
instance ToSchema TranslationsResponse

data TranslationBooks = TranslationBooks
  { translation :: Translation
  , books :: [TranslationBook]
  } deriving (Show, Eq, Generic)

instance FromJSON TranslationBooks
instance ToJSON TranslationBooks
instance ToSchema TranslationBooks

data TranslationBook = TranslationBook
  { id :: String
  , name :: String
  , commonName :: String
  , title :: Maybe String
  , order :: Int
  , numberOfChapters :: Int
  , firstChapterNumber :: Int
  , firstChapterApiLink :: String
  , lastChapterNumber :: Int
  , lastChapterApiLink :: String
  , totalNumberOfVerses :: Int
  , isApocryphal :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON TranslationBook
instance ToJSON TranslationBook
instance ToSchema TranslationBook

data TranslationBookChapter = TranslationBookChapter
  { translation :: Translation
  , book :: TranslationBook
  , thisChapterLink :: String
  , thisChapterAudioLinks :: TranslationBookChapterAudioLinks
  , nextChapterApiLink :: Maybe String
  , nextChapterAudioLinks :: Maybe TranslationBookChapterAudioLinks
  , previousChapterApiLink :: Maybe String
  , previousChapterAudioLinks :: Maybe TranslationBookChapterAudioLinks
  , numberOfVerses :: Int
  , chapter :: ChapterData
  } deriving (Show, Eq, Generic)

instance FromJSON TranslationBookChapter
instance ToJSON TranslationBookChapter
instance ToSchema TranslationBookChapter

data ChapterData = ChapterData
  { number :: Int
  , content :: [ChapterContent]
  , footnotes :: [ChapterFootnote]  
  } deriving (Show, Eq, Generic)

instance FromJSON ChapterData
instance ToJSON ChapterData
instance ToSchema ChapterData

data ChapterContent
  = ChapterHeading
      { type_ :: String
      , headdingContent :: [String]
      }
  | ChapterLineBreak
      { type_ :: String
      }
  | ChapterHebrewSubtitle
      { type_ :: String
      , subtitleContent :: [HebrewSubtitleContent]
      }
  | ChapterVerse
      { type_ :: String
      , number :: Int
      , verseContent :: [VerseContent]
      }
  deriving (Show, Eq, Generic)

instance FromJSON ChapterContent where
  parseJSON = withObject "ChapterContent" $ \o -> do
    t <- o .: "type" :: Parser String
    case t of
      "heading" ->
        ChapterHeading
          <$> o .: "type"
          <*> o .: "content"

      "line_break" ->
        ChapterLineBreak
          <$> o .: "type"

      "hebrew_subtitle" ->
        ChapterHebrewSubtitle
          <$> o .: "type"
          <*> o .: "content"

      "verse" ->
        ChapterVerse
          <$> o .: "type"
          <*> o .: "number"
          <*> o .: "content"

      _ -> fail ("Unknown ChapterContent type: " ++ t)
instance ToJSON ChapterContent where
  toJSON (ChapterHeading _ headdingContent) =
    object
      [ "type" .= String "heading"
      , "content" .= headdingContent
      ]
  toJSON (ChapterLineBreak _) =
    object
      [ "type" .= String "line_break"
      ]
  toJSON (ChapterHebrewSubtitle _ subtitleContent) =
    object
      [ "type" .= String "hebrew_subtitle"
      , "content" .= subtitleContent
      ]
  toJSON (ChapterVerse _ n verseContent) =
    object
      [ "type" .= String "verse"
      , "number" .= n
      , "content" .= verseContent
      ]
instance ToSchema ChapterContent

data HebrewSubtitleContent
  = HebrewSubtitleString String
  | HebrewSubtitleFormattedText FormattedText
  | HebrewSubtitleVerseFootnoteReference VerseFootnoteRef
  deriving (Show, Eq, Generic)

instance FromJSON HebrewSubtitleContent

instance ToJSON HebrewSubtitleContent where
  toJSON (HebrewSubtitleString s) = String (T.pack s)
  toJSON (HebrewSubtitleFormattedText t) = toJSON t
  toJSON (HebrewSubtitleVerseFootnoteReference r) = toJSON r

instance ToSchema HebrewSubtitleContent

data VerseContent
  = VerseString String
  | VerseFormattedText FormattedText
  | VerseInlineHeading InlineHeading
  | VerseInlineLineBreak InlineLineBreak
  | VerseFootnoteReference VerseFootnoteRef
  deriving (Show, Eq, Generic)

instance FromJSON VerseContent where
  parseJSON v =
        (VerseString <$> parseJSON v)
    <|> (VerseFormattedText <$> parseJSON v)
    <|> (VerseInlineHeading <$> parseJSON v)
    <|> (VerseInlineLineBreak <$> parseJSON v)
    <|> (VerseFootnoteReference <$> parseJSON v)

instance ToJSON VerseContent where
  toJSON (VerseString s) = String (T.pack s)
  toJSON (VerseFormattedText t) = toJSON t
  toJSON (VerseInlineHeading h) = toJSON h
  toJSON (VerseInlineLineBreak b) = toJSON b
  toJSON (VerseFootnoteReference r) = toJSON r

instance ToSchema VerseContent

data FormattedText = FormattedText
  { text :: String
  , poem :: Maybe Int
  , wordsOfJesus :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON FormattedText
instance ToJSON FormattedText
instance ToSchema FormattedText

data InlineHeading = InlineHeading
  { heading :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON InlineHeading
instance ToJSON InlineHeading
instance ToSchema InlineHeading

data InlineLineBreak = InlineLineBreak
  { lineBreak :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON InlineLineBreak 
instance ToJSON InlineLineBreak 
instance ToSchema InlineLineBreak

data VerseFootnoteRef = VerseFootnoteRef
  { noteId :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON VerseFootnoteRef
instance ToJSON VerseFootnoteRef where
  toJSON (VerseFootnoteRef n) =
    object [ "noteId" .= n ]
instance ToSchema VerseFootnoteRef

data ChapterFootnote = ChapterFootnote
  { noteId :: Int
  , text :: String
  , reference :: Maybe ChapterFootnoteReference
  , caller :: ChapterFootnoteCaller
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChapterFootnote
instance ToJSON ChapterFootnote
instance ToSchema ChapterFootnote

data ChapterFootnoteReference = ChapterFootnoteReference
  { chapter :: Int
  , verse :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChapterFootnoteReference
instance ToJSON ChapterFootnoteReference where
  toJSON (ChapterFootnoteReference c v) =
    object
      [ "chapter" .= c
      , "verse" .= v
      ]
instance ToSchema ChapterFootnoteReference

data ChapterFootnoteCaller
  = CallerPlus
  | CallerText String
  | CallerNull
  deriving (Show, Eq, Generic)

instance FromJSON ChapterFootnoteCaller where
  parseJSON Null = pure CallerNull
  parseJSON (String "+") = pure CallerPlus
  parseJSON (String s) = pure (CallerText (T.unpack s))
  parseJSON _ = fail "Invalid ChapterFootnoteCaller"
instance ToJSON ChapterFootnoteCaller where
  toJSON CallerNull = Null
  toJSON CallerPlus = String "+"
  toJSON (CallerText s) = String (T.pack s)
instance ToSchema ChapterFootnoteCaller where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

type TranslationBookChapterAudioLinks = Map String String

data HTMLLucid
instance Accept HTMLLucid where
  contentType _ = "text" Network.HTTP.Media.// "html"

instance MimeRender HTMLLucid [Translation] where
  mimeRender _ translations = renderBS (toHtml translations)

comparePage :: Monad m => HtmlT m ()
comparePage = doctypehtml_ $ do
  head_ $ do
    title_ "Bible tools"
    meta_ [charset_ "utf-8"]

  body_ $ do
    h1_ "Bible tools"

    h2_ "Compare translations"

    p_ $ do
      label_ [for_ "translation1"] "Translation 1: "
      input_ [Lucid.type_ "text", id_ "translation1", value_ "BSB"]

    p_ $ do
      label_ [for_ "translation2"] "Translation 2: "
      input_ [Lucid.type_ "text", id_ "translation2", value_ "ENGWEBP"]

    p_ $ do
      label_ [for_ "book"] "Book: "
      input_ [Lucid.type_ "text", id_ "book", value_ "GEN"]

    p_ $ do
      label_ [for_ "chapter"] "Chapter: "
      input_ [Lucid.type_ "number", id_ "chapter", value_ "1", min_ "1"]

    p_ $ do
      button_
        [ Lucid.type_ "button"
        , onclick_ $
            "window.location.href = '/compare/' + "
            <> "encodeURIComponent(document.getElementById('translation1').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('translation2').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('book').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('chapter').value);"
        ]
        "Compare"

    hr_ []

    h2_ "Get chapter"

    p_ $ do
      label_ [for_ "ch_translation"] "Translation: "
      input_ [Lucid.type_ "text", id_ "ch_translation", value_ "BSB"]

    p_ $ do
      label_ [for_ "ch_book"] "Book: "
      input_ [Lucid.type_ "text", id_ "ch_book", value_ "GEN"]

    p_ $ do
      label_ [for_ "ch_chapter"] "Chapter: "
      input_ [Lucid.type_ "number", id_ "ch_chapter", value_ "1", min_ "1"]

    p_ $ do
      button_
        [ Lucid.type_ "button"
        , onclick_ $
            "window.location.href = '/chapter/' + "
            <> "encodeURIComponent(document.getElementById('ch_translation').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('ch_book').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('ch_chapter').value);"
        ]
        "Get Chapter"

    hr_ []

    h2_ "Get verse"

    p_ $ do
      label_ [for_ "v_translation"] "Translation: "
      input_ [Lucid.type_ "text", id_ "v_translation", value_ "BSB"]

    p_ $ do
      label_ [for_ "v_book"] "Book: "
      input_ [Lucid.type_ "text", id_ "v_book", value_ "GEN"]

    p_ $ do
      label_ [for_ "v_chapter"] "Chapter: "
      input_ [Lucid.type_ "number", id_ "v_chapter", value_ "1", min_ "1"]

    p_ $ do
      label_ [for_ "v_verse"] "Verse: "
      input_ [Lucid.type_ "number", id_ "v_verse", value_ "1", min_ "1"]

    p_ $ do
      button_
        [ Lucid.type_ "button"
        , onclick_ $
            "window.location.href = '/verse/' + "
            <> "encodeURIComponent(document.getElementById('v_translation').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('v_book').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('v_chapter').value) + '/' + "
            <> "encodeURIComponent(document.getElementById('v_verse').value);"
        ]
        "Get Verse"

instance ToHtml [Translation] where
  toHtml tranlations = comparePage
  toHtmlRaw = toHtml

data ChapterDiff = ChapterDiff
  { translationName1 :: String
  , translationName2 :: String
  , book :: String
  , chapter :: String
  , verseDiffs :: [VerseDiff]
  } deriving (Show, Eq, Generic)

instance ToJSON ChapterDiff
instance ToSchema ChapterDiff
instance ToSchema (PolyDiff a b) where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "PolyDiff") mempty
data VerseDiff = VerseDiff
  { translationText1 :: String
  , translationText2 :: String
  , diff :: [Diff String]
  } deriving (Show, Eq, Generic)

instance ToJSON VerseDiff
instance (ToJSON a, ToJSON b) => ToJSON (PolyDiff a b) where
  toJSON (First a)   = object [ "tag" .= String "First",  "value" .= a ]
  toJSON (Second b)  = object [ "tag" .= String "Second", "value" .= b ]
  toJSON (Both a b)  = object [ "tag" .= String "Both",   "left"  .= a, "right" .= b ]
instance ToSchema VerseDiff

data Chapter = Chapter
  { translation :: String
  , book :: String
  , chapter :: String
  , verses :: [String]
  } deriving (Show, Eq, Generic)

instance ToJSON Chapter
instance ToSchema Chapter

data Verse = Verse
  { translation :: String
  , book :: String
  , chapter :: String
  , verseNumber :: Int
  , verseText :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Verse
instance ToSchema Verse

fetchBibleChapter :: String -> String -> Int -> IO (Either ClientError TranslationBookChapter)
fetchBibleChapter translation book chapterNum = do
  manager' <- newTlsManager
  runClientM
    (bookChapter translation book (show chapterNum ++ ".json"))
    (mkClientEnv manager' bibleBaseUrl)

chapterVersePairs :: [ChapterContent] -> [(Int, String)]
chapterVersePairs cs =
  [ (n, concat [piece vc | vc <- verseContent])
  | ChapterVerse{number = n, verseContent} <- cs
  ]
  where
    piece :: VerseContent -> String
    piece (VerseString s) = s
    piece (VerseFormattedText FormattedText{text = t}) = t
    piece (VerseInlineLineBreak _) = " "
    piece (VerseInlineHeading _) = " "
    piece (VerseFootnoteReference _) = ""

chapterPage :: Monad m => Chapter -> HtmlT m ()
chapterPage Chapter{translation, book, chapter, verses} =
  doctypehtml_ $ do
    head_ $ do
      title_ "Chapter"
      meta_ [charset_ "utf-8"]
      style_ $
        "body { font-family: sans-serif; margin: 2rem; }\
        \ .meta { margin-bottom: 1.5rem; }\
        \ .meta div { margin: 0.25rem 0; }\
        \ .verse { margin: 0.5rem 0; padding: 0.75rem 1rem; border: 1px solid #ddd; border-radius: 8px; }"

    body_ $ do
      h1_ "Chapter"
      div_ [class_ "meta"] $ do
        div_ $ strong_ "Translation: " >> toHtml translation
        div_ $ strong_ "Book: " >> toHtml book
        div_ $ strong_ "Chapter: " >> toHtml chapter

      h2_ "Verses"
      ol_ $
        forM_ (zip [1 :: Int ..] verses) $ \(n, v) ->
          li_ [class_ "verse"] $ do
            toHtml v

instance ToHtml Chapter where
  toHtml = chapterPage

instance MimeRender HTMLLucid Chapter where
  mimeRender _ = renderBS . toHtml

versePage :: Monad m => Verse -> HtmlT m ()
versePage Verse{translation, book, chapter, verseNumber, verseText} =
  doctypehtml_ $ do
    head_ $ do
      title_ "Verse"
      meta_ [charset_ "utf-8"]
      style_ $
        "body { font-family: sans-serif; margin: 2rem; }\
        \ .meta { margin-bottom: 1.5rem; }\
        \ .meta div { margin: 0.25rem 0; }\
        \ .verse-box { margin-top: 1rem; padding: 1rem; border: 1px solid #ddd; border-radius: 8px; }"

    body_ $ do
      h1_ "Verse"
      div_ [class_ "meta"] $ do
        div_ $ strong_ "Translation: " >> toHtml translation
        div_ $ strong_ "Book: " >> toHtml book
        div_ $ strong_ "Chapter: " >> toHtml chapter
        div_ $ strong_ "Verse: " >> toHtml (show verseNumber)

      div_ [class_ "verse-box"] $
        p_ $ toHtml verseText

instance ToHtml Verse where
  toHtml = versePage

instance MimeRender HTMLLucid Verse where
  mimeRender _ = renderBS . toHtml

chapterDiffPage :: Monad m => ChapterDiff -> HtmlT m ()
chapterDiffPage ChapterDiff{translationName1, translationName2, book, chapter, verseDiffs} =
  doctypehtml_ $ do
    head_ $ do
      title_ "Chapter Diff"
      meta_ [charset_ "utf-8"]
      style_ $
        "body { font-family: sans-serif; margin: 2rem; }\
        \ .meta { margin-bottom: 1.5rem; }\
        \ .meta div { margin: 0.25rem 0; }\
        \ .verse { margin: 1rem 0; padding: 1rem; border: 1px solid #ddd; border-radius: 8px; }\
        \ .verse-head { font-weight: bold; margin-bottom: 0.5rem; }\
        \ .line { margin: 0.25rem 0; }\
        \ .first { color: #666; }\
        \ .second { color: #666; }\
        \ .both { color: #111; }\
        \ .del { text-decoration: line-through; opacity: 0.7; }\
        \ .add { font-weight: 600; }"

    body_ $ do
      h1_ "Chapter Diff"

      div_ [class_ "meta"] $ do
        div_ $ do strong_ "Translation 1: " >> toHtml translationName1
        div_ $ do strong_ "Translation 2: " >> toHtml translationName2
        div_ $ do strong_ "Book: " >> toHtml book
        div_ $ do strong_ "Chapter: " >> toHtml chapter

      h2_ "Verses"

      mapM_ renderVerseDiff verseDiffs

renderVerseDiff :: Monad m => VerseDiff -> HtmlT m ()
renderVerseDiff VerseDiff{translationText1, translationText2, diff} =
  div_ [class_ "verse"] $ do
    div_ [class_ "verse-head"] $
      toHtml translationText1 <> " / " <> toHtml translationText2

    div_ [class_ "line"] $ do
      strong_ "Diff: "
      renderDiff diff

renderDiff :: Monad m => [PolyDiff String String] -> HtmlT m ()
renderDiff = mapM_ renderPiece

renderPiece :: Monad m => PolyDiff String String -> HtmlT m ()
renderPiece (First s) =
  span_ [class_ "first del"] (toHtml s)
renderPiece (Second s) =
  span_ [class_ "second add"] (toHtml s)
renderPiece (Both a b)
  | a == b    = span_ [class_ "both"] (toHtml a)
  | otherwise = span_ [class_ "both"] (toHtml a <> " | " <> toHtml b)


instance MimeRender HTMLLucid ChapterDiff where
  mimeRender _ = renderBS . toHtml

instance ToHtml ChapterDiff where
  toHtml = chapterDiffPage
  
chapterContentsToString :: [ChapterContent] -> [String]
chapterContentsToString cs =
  [ concat [ case vc of
      VerseString s -> s
      VerseFormattedText FormattedText{text = t} -> t
      VerseInlineLineBreak _ -> " "
      VerseInlineHeading _ -> " "
      _ -> ""
    | vc <- verseContent ] | ChapterVerse{verseContent} <- cs ]
  
diffVerses :: String -> String -> String -> Int -> IO (Either ServerError ChapterDiff)
diffVerses translation1 translation2 book chapter = do
  manager' <- newTlsManager
  let env = mkClientEnv manager' bibleBaseUrl
      fetch t = runClientM (bookChapter t book (show chapter ++ ".json")) env

  (validRes, (res1, res2)) <-
    concurrently
      (runClientM availableTranslations env)
      (concurrently (fetch translation1) (fetch translation2))

  case validRes of
    Left err -> do
      print err
      pure $ Left err502 { errBody = "Could not validate translations" }

    Right (TranslationsResponse ts) -> do
      let exists t = any (\tr -> (\Translation{id = i} -> i) tr == t) ts

      if not (exists translation1)
        then pure $ Left err400 { errBody = "Invalid translation1" }
      else if not (exists translation2)
        then pure $ Left err400 { errBody = "Invalid translation2" }
      else
        case (res1, res2) of
          (Left err1, _) -> do
            print err1
            pure $ Left err502 { errBody = "Could not find book or chapter in translation1" }

          (_, Left err2) -> do
            print err2
            pure $ Left err502 { errBody = "Could not find book or chapter in translation2" }

          (Right t1, Right t2) ->
            pure $ Right ChapterDiff
              { translationName1 = englishName . (\TranslationBookChapter{translation = t} -> t) $ t1
              , translationName2 = englishName . (\TranslationBookChapter{translation = t} -> t) $ t2
              , book = (\TranslationBook{name = n} -> n) . (\TranslationBookChapter{book = b} -> b) $ t1
              , chapter = show . (\ChapterData{number = n} -> n) . (\TranslationBookChapter{chapter = c} -> c) $ t1
              , verseDiffs = zipWith (\x y -> VerseDiff {translationText1 = x, translationText2 = y, diff = getGroupedDiff x y}) (chapterContentsToString . (\ChapterData{content = c} -> c) . (\TranslationBookChapter{chapter = c} -> c) $ t1) (chapterContentsToString . (\ChapterData{content = c} -> c) . (\TranslationBookChapter{chapter = c} -> c) $ t2)
              }

bibleBaseUrl :: BaseUrl
bibleBaseUrl = BaseUrl Https "bible.helloao.org" 443 "/api"

type BibleAPI = "available_translations.json" :> Get '[JSON] TranslationsResponse
           :<|> Capture "translation" String :> "books.json" :> Get '[JSON] TranslationBooks
           :<|> Capture "translation" String :> Capture "book" String :> Capture "chapter" String :> Get '[JSON] TranslationBookChapter

bibleApi :: Proxy BibleAPI
bibleApi = Proxy

availableTranslations :<|> booksInTranslation :<|> bookChapter = client bibleApi

type APIWithoutDocs = Get '[JSON, HTMLLucid] [Translation]
      :<|> "compare" :> Capture "translation1" String :> Capture "translation2" String :> Capture "book" String :> Capture "chapter" Int :> Get '[JSON, HTMLLucid] ChapterDiff
      :<|> "chapter" :> Capture "translation" String :> Capture "book" String :> Capture "chapter" Int :> Get '[JSON, HTMLLucid] Chapter
      :<|> "verse"   :> Capture "translation" String :> Capture "book" String :> Capture "chapter" Int :> Capture "verse" Int :> Get '[JSON, HTMLLucid] Verse
      
type API = APIWithoutDocs :<|> SwaggerSchemaUI "docs" "docs.json"
      
server :: Server API
server = (translations :<|> compare :<|> chapterHandler :<|> verseHandler) :<|> docs
         where translations :: Handler [Translation] 
               translations = do
                 manager' <- newTlsManager
                 res <- liftIO $ runClientM availableTranslations (mkClientEnv manager' bibleBaseUrl)
                 case res of
                   Left err -> throwError err502 { errBody = "Could not get translations from server" }  
                   Right (TranslationsResponse ts) -> return ts
               compare :: String -> String -> String -> Int -> Handler ChapterDiff
               compare translation1 translation2 book chapter = do
                 res <- liftIO $ diffVerses translation1 translation2 book chapter  
                 case res of
                   Left err -> do
                     liftIO $ print err
                     throwError err
                   Right t -> return t
               chapterHandler :: String -> String -> Int -> Handler Chapter
               chapterHandler translation book chapterNum = do
                 res <- liftIO $ fetchBibleChapter translation book chapterNum
                 case res of
                   Left err -> do
                     liftIO $ print err
                     throwError err502 { errBody = "Could not get chapter from server" }
                   Right t -> do
                     let versesText = chapterContentsToString . content . (\TranslationBookChapter{chapter = c} -> c) $ t
                     pure Chapter
                       { translation = translation
                       , book = (\TranslationBook{name = n} -> n) . (\TranslationBookChapter{book = b} -> b) $ t
                       , chapter = show . (\ChapterData{number = n} -> n) . (\TranslationBookChapter{chapter = c} -> c) $ t
                       , verses = versesText
                       }

               verseHandler :: String -> String -> Int -> Int -> Handler Verse
               verseHandler translation book chapterNum verseNum = do
                 res <- liftIO $ fetchBibleChapter translation book chapterNum
                 case res of
                   Left err -> do
                     liftIO $ print err
                     throwError err502 { errBody = "Could not get verse from server" }
                   Right t -> do
                     let pairs = chapterVersePairs . content . (\TranslationBookChapter{chapter = c} -> c) $ t
                     case lookup verseNum pairs of
                       Nothing ->
                         throwError err404 { errBody = "Verse not found" }
                       Just verseText ->
                         pure Verse
                         { translation = translation
                         , book = (\TranslationBook{name = n} -> n) . (\TranslationBookChapter{book = b} -> b) $ t
                         , chapter = show . (\ChapterData{number = n} -> n) . (\TranslationBookChapter{chapter = c} -> c) $ t
                         , verseNumber = verseNum
                         , verseText = verseText
                         }
               docs = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy APIWithoutDocs)) 
                 

api :: Proxy API
api = Proxy

app = serve api server
      
main = run 8080 app
