{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (&&&), (***), (+++), (|||), arr)
import Data.Monoid (mempty, mconcat)
import Data.Either (rights)
import Data.Maybe
import Data.Time
import Data.Time.Format
import Locale

import Hakyll

instance Writable b => Writable (Either a b) where
  write p (Right b) = write p b
  write _ _ = return ()

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ (pageCompiler &&& (unsafeCompiler (\_ -> getCurrentTime)))
	    >>> isPagePublishedYet
	    >>> (id +++ (applyTemplateCompiler "templates/post.html"
                         >>> applyTemplateCompiler "templates/default.html"
                         >>> relativizeUrlsCompiler))

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (filterPublishedE >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (filterPublishedE >>> (id *** arr (take 3 . reverse . sortByBaseName)) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

isPublished :: Page a -> Bool
isPublished p =
  let published = getField "published" p in
  published /= "" && published /= "false"       

isPagePublished :: Compiler (Page a) (Either (Page a) (Page a))
isPagePublished = arr (\p -> if isPublished p then Right p else Left p)

filterPublished :: Compiler (Page a, [Page b]) (Page a, [Page b])
filterPublished = id *** arr (filter isPublished)

filterPublishedE :: Compiler (Page a, [Either (Page b) (Page b)]) (Page a, [Page b])
filterPublishedE = id *** arr rights

isPublishedYet :: Page a -> UTCTime -> Bool
isPublishedYet page time =  
    let published = getField "published" page in
    published == "true" || past published
    where
    past published =
        let publishAt = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" published in
        fromMaybe False (fmap (\embargo -> embargo < time) publishAt)

isPagePublishedYet :: Compiler (Page a, UTCTime) (Either (Page a) (Page a))
isPagePublishedYet = arr (\(p,t) -> if isPublishedYet p t then pub p else unpub p)
  where
    pub p = Right $ setField "published" "true" p
    unpub p = Left $ setField "published" "false" p

