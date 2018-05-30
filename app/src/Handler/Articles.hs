{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.Articles
  ( getArticlesR
  )
  where

import           Data.Aeson
import qualified Data.Text          as T
import           Data.Text.Read     (decimal)
import           Database.Esqueleto ((==.), (?.), (^.))
import qualified Database.Esqueleto as E
import           Handler.Profiles   (encodeProfile)
import           Import             hiding ((==.))


--------------------------------------------------------------------------------
-- List Articles

getArticlesR :: Handler Value
getArticlesR = do
  mCurrentUserId <- maybeAuthId
  mTag <- lookupGetParam "tag"
  mFilterAuthor <- lookupGetParam "author"
  mFilterFavoritedBy <- lookupGetParam "favorited"
  mLimit <- lookupGetParam "limit"
  mOffset <- lookupGetParam "offset"
  let limit = decimalWithDefault mLimit 20
      offset = decimalWithDefault mOffset 0
      hasTag article =
        case mTag of
          Just filterTag ->
            E.exists $
            E.from $ \(tag `E.InnerJoin` articleTag) -> do
              E.on $ tag ^. TagId ==. articleTag ^. ArticleTagTag
              E.where_ $
                articleTag ^. ArticleTagArticle ==. article ^. ArticleId
              E.where_ $ tag ^. TagName ==. E.val filterTag
          _ -> E.val True
      filterAuthor author =
        case mFilterAuthor of
          Just filterAuthor -> E.val filterAuthor ==. author ^. UserUsername
          _                 -> E.val True
      filterFavoritedBy =
        case mFilterFavoritedBy of
          Just favoritedBy ->
            E.exists $
            E.from $ \(favorite `E.InnerJoin` user) -> do
              E.on $ favorite ^. ArticleFavoriteUser ==. user ^. UserId
              E.where_ $ E.val favoritedBy ==. user ^. UserUsername
          _ -> E.val True
  articles <-
    runDB $
    E.select $
    E.from $ \(article
               `E.InnerJoin` author
               `E.LeftOuterJoin` mFollower
               `E.LeftOuterJoin` mFavourite) -> do
      E.on $ article ^. ArticleAuthor ==. author ^. UserId
      E.on $ mFollower ?. UserFollowerUser ==. E.just (author ^. UserId)
      E.on $ mFollower ?. UserFollowerFollower ==. E.val mCurrentUserId
      E.on $ mFavourite ?. ArticleFavoriteUser ==.  E.val mCurrentUserId
      E.limit limit
      E.offset offset
      E.where_ $ hasTag article
      E.where_ $ filterAuthor author
      E.where_ filterFavoritedBy
      let following = E.not_ $ E.isNothing $ mFollower ?. UserFollowerId
          favorited = E.not_ $ E.isNothing $ mFavourite ?. ArticleFavoriteId
          favoritesCount =
            E.sub_select $
            E.from $ \favorite -> do
              E.where_ $
                favorite ^. ArticleFavoriteArticle ==. article ^. ArticleId
              return E.countRows
      return (article, author, following, favorited, favoritesCount)
  articlesWithTags <- mapM selectArticleTagList articles
  return $ object ["articles" .= articlesWithTags]

selectArticleTagList ::
     ( Entity Article, Entity User, E.Value Bool
     , E.Value Bool, E.Value Int
     )
  -> Handler Value
selectArticleTagList
  ( Entity articleId Article {..}, Entity _ author, E.Value following
  , E.Value favorited , E.Value favoritesCount
  ) = do
  tags <-
    runDB $
    E.select $
    E.from $ \(articleTag `E.InnerJoin` tag) -> do
      E.where_ $ articleTag ^. ArticleTagArticle ==. E.val articleId
      E.where_ $ tag ^. TagId ==. articleTag ^. ArticleTagTag
      return $ tag ^. TagName
  return $ object
      [ "slug" .= articleSlug
      , "title" .= articleTitle
      , "description" .= articleDescription
      , "body" .= articleBody
      , "tagList" .= (E.unValue <$> tags :: [Text])
      , "createdAt" .= articleCreatedAt
      , "updatedAt" .= articleUpdatedAt
      , "favorited" .= favorited
      , "favoritesCount" .= favoritesCount
      , "author" .= encodeProfile author following
      ]

--------------------------------------------------------------------------------
-- Helpers

slug :: Text -> Text
slug = toLower . T.map h
  where
    h ' ' = '-'
    h c   = c

decimalWithDefault :: Integral p => Maybe Text -> p -> p
decimalWithDefault x default' =
        case decimal <$> x of
          Just (Right (l, _)) -> l
          _                   -> default'
