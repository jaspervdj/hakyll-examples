{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import           Control.Arrow                   (arr, second, (&&&), (>>>))
import           Control.Category                (id)
import           Control.Monad                   (forM_)
import           Data.Maybe                      (fromMaybe)
import           Prelude                         hiding (id)
import           Text.Blaze.Html                 (Html, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import           Hakyll


-- | Converts the page path, e.g. 'en/about.html' to the 'en' language code and
-- the 'about.html' path.
getLanguageCode :: Compiler (Page a) (String, String)
getLanguageCode = getRoute >>>
    arr (fmap splitLanguageCode) >>>
    arr (fromMaybe ("en", ""))
  where
    splitLanguageCode = second (drop 1) . break (== '/')

-- | List of all supported languages
languages :: [(String, String)]
languages =
    [ ("en", "English")
    , ("nl", "Dutch")
    , ("fr", "Français")
    ]

-- | Generate an HTML menu for all languages except the current one using
-- blaze-html.
languageNavigation :: String -> String -> Html
languageNavigation code path =
    forM_ translations $ \(c, name) ->
        H.a ! A.href (H.toValue $ "/" ++ c ++ "/" ++ path) $
            H.toHtml name
  where
    translations = filter ((/= code) . fst) languages

-- | Compiler version of 'languageNavigation'
getLanguageNavigation :: Compiler (Page a) String
getLanguageNavigation = getLanguageCode >>>
    arr (uncurry languageNavigation) >>>
    arr renderHtml

-- | Sets $languages$ to the language navigation menu en $language$ to the
-- current language code.
addLanguages :: Compiler (Page a) (Page a)
addLanguages =
    (getLanguageCode >>> arr fst) &&& id >>>
    arr (uncurry (setField "language")) >>>
    getLanguageNavigation &&& id >>>
    arr (uncurry (setField "languages"))

main :: IO ()
main = hakyll $ do
    match "**.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> addLanguages
            >>> applyTemplateCompiler "templates/default.html"

    match "templates/*" $ compile templateCompiler

    match "style/*.css" $ do
        route idRoute
        compile compressCssCompiler
