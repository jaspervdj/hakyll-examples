{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import Hakyll
import Text.Pandoc (ParserState (..))

main :: IO ()
main = hakyll $ do
    match "*.markdown" $ do
        route $ setExtension "html"
        compile $
            readPageCompiler >>>
            addDefaultFields >>>
            pageReadPandocBiblio defaultHakyllParserState
                "default.csl" "references.bib" >>>
            arr (fmap writePandoc)

    match "default.csl"    $ compile cslCompiler
    match "references.bib" $ compile biblioCompiler
