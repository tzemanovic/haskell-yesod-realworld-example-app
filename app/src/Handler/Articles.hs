{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Articles
  ( getArticlesR
  , getArticlesFeedR
  , getArticleR
  , postArticlesR
  , putArticleR
  , deleteArticleR
  , getArticleCommentsR
  , postArticleCommentsR
  , deleteArticleCommentR
  , postArticleFavoriteR
  , deleteArticleFavoriteR
  , getTagsR
  ) where

import           Data.Aeson
import qualified Data.Text                 as T
import qualified Database
import           Database.Persist.Extended (maybeUpdate)
import           Database.Persist.Sql      (toSqlKey)
import           Import
import           Pagination
import           Test.QuickCheck.Gen       (elements, generate, listOf)
import           Web.Forma.Extended


--------------------------------------------------------------------------------
-- List Articles

getArticlesR :: Handler Value
getArticlesR = do
  mCurrentUserId <- maybeAuthId
  mTag <- lookupGetParam "tag"
  mFilterAuthor <- lookupGetParam "author"
  mFilterFavoritedBy <- lookupGetParam "favorited"
  page <- lookupPageParams
  (articles, articlesCount) <-
    Database.getGlobalArticleFeed
      mCurrentUserId
      mTag
      mFilterAuthor
      mFilterFavoritedBy
      page

  return $ object ["articles" .= articles, "articlesCount" .= articlesCount]

--------------------------------------------------------------------------------
-- Article feed

getArticlesFeedR :: Handler Value
getArticlesFeedR = do
  mCurrentUserId <- maybeAuthId
  page <- lookupPageParams
  (articles, articlesCount) <- Database.getUserArticleFeed mCurrentUserId page

  return $ object ["articles" .= articles, "articlesCount" .= articlesCount]

--------------------------------------------------------------------------------
-- Get article

getArticleR :: Text -> Handler Value
getArticleR slug = do
  mArticle <- runDB $ getBy $ UniqueArticleSlug slug
  case mArticle of
    Just (Entity articleId _) -> getArticle articleId
    _                         -> notFound

--------------------------------------------------------------------------------
-- Create article

type ArticleFields = '[ "article", "title", "description", "body", "tagList" ]

data CreateArticle = CreateArticle
  { createArticleTitle       :: Text
  , createArticleDescription :: Text
  , createArticleBody        :: Text
  , createArticleTagList     :: Maybe [Text]
  } deriving (Show)

createArticleForm :: FormParser ArticleFields Text Handler CreateArticle
createArticleForm =
  subParser #article (CreateArticle
    <$> field #title notEmpty
    <*> field #description notEmpty
    <*> field #body notEmpty
    <*> optional (field' #tagList))

postArticlesR :: Handler Value
postArticlesR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId ->
      withForm createArticleForm $ \CreateArticle {..} -> do
        now <- liftIO getCurrentTime
        slug <- liftIO $ toSlug createArticleTitle
        let article =
              Article
                userId
                createArticleTitle
                slug
                createArticleDescription
                createArticleBody
                now
                now

        articleId <- runDB $ insert article

        forM_ createArticleTagList $
          mapM_ $ \tag -> do
            Entity tagId _ <- runDB $ upsert (Tag tag) []
            runDB $ insert_ $ ArticleTag articleId tagId

        getArticle articleId

--------------------------------------------------------------------------------
-- Update article

data UpdateArticle = UpdateArticle
  { updateArticleTitle       :: Maybe Text
  , updateArticleDescription :: Maybe Text
  , updateArticleBody        :: Maybe Text
  } deriving Show

updateArticleForm :: FormParser ArticleFields Text Handler UpdateArticle
updateArticleForm =
  subParser #article (UpdateArticle
    <$> optional (field #title notEmpty)
    <*> optional (field #description notEmpty)
    <*> optional (field #body notEmpty))

putArticleR :: Text -> Handler Value
putArticleR slug = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug

      case mArticle of

        Just article@(Entity _ Article {..}) ->
          if articleAuthor /= userId
            then permissionDenied "Unauthorized"
            else updateArticle article

        _  ->
          notFound

updateArticle :: Entity Article -> Handler Value
updateArticle (Entity articleId Article {..}) =
  withForm updateArticleForm $ \UpdateArticle {..} -> do
    now <- liftIO getCurrentTime
    updateSlug <-
      case updateArticleTitle of

        Just updateTitle | updateTitle /= articleTitle ->
          Just <$> liftIO (toSlug updateTitle)

        _ ->
          return Nothing

    let updates =
          catMaybes
            [ maybeUpdate ArticleTitle updateArticleTitle
            , maybeUpdate ArticleSlug updateSlug
            , maybeUpdate ArticleDescription updateArticleDescription
            , maybeUpdate ArticleBody updateArticleBody
            , maybeUpdate ArticleUpdatedAt (Just now)
            ]
    runDB $ update articleId updates
    getArticle articleId

--------------------------------------------------------------------------------
-- Delete article

deleteArticleR :: Text -> Handler Value
deleteArticleR slug = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug
      case mArticle of

        Just (Entity articleId Article {..}) ->
          if articleAuthor /= userId
            then permissionDenied "Unauthorized"
            else deleteArticle articleId

        _  ->
          notFound

deleteArticle :: ArticleId -> Handler Value
deleteArticle articleId = do
  runDB $ deleteCascade articleId
  return Null

--------------------------------------------------------------------------------
-- Get article's comments

getArticleCommentsR :: Text -> Handler Value
getArticleCommentsR slug = do
  mUserId <- maybeAuthId
  comments <- Database.getCommentsByArticleSlug mUserId slug
  return $ object ["comments" .= comments]

--------------------------------------------------------------------------------
-- Add comment to article

type CommentFields = '[ "comment", "body" ]

newtype CreateComment = CreateComment
  { createCommentBody :: Text
  } deriving Show

createCommentForm :: FormParser CommentFields Text Handler CreateComment
createCommentForm =
  subParser #comment $ CreateComment
    <$> field #body notEmpty

postArticleCommentsR :: Text -> Handler Value
postArticleCommentsR slug = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug
      case mArticle of

        Just (Entity articleId _) ->
          withForm createCommentForm $ \CreateComment {..} -> do
            now <- liftIO getCurrentTime
            let comment =
                  ArticleComment
                    articleId
                    userId
                    createCommentBody
                    now
                    now
            commentId <- runDB $ insert comment
            getComment commentId

        _  ->
          notFound

--------------------------------------------------------------------------------
-- Delete article's comment

deleteArticleCommentR :: Text -> Int -> Handler Value
deleteArticleCommentR slug commentId = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug
      case mArticle of

        Just (Entity _ Article {..}) ->
          if articleAuthor == userId
            then do
              runDB $
                deleteWhere [ArticleCommentId ==. toSqlKey (fromIntegral commentId)]
              return Null
            else permissionDenied "Unauthorized"

        _  ->
          notFound

--------------------------------------------------------------------------------
-- Favorite article

postArticleFavoriteR :: Text -> Handler Value
postArticleFavoriteR slug = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug
      case mArticle of

        Just (Entity articleId _) -> do
          let articleFavorite = ArticleFavorite articleId userId
          void $ runDB $ insertUnique articleFavorite
          getArticle articleId

        _  ->
          notFound

--------------------------------------------------------------------------------
-- Unfavorite article

deleteArticleFavoriteR :: Text -> Handler Value
deleteArticleFavoriteR slug = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mArticle <- runDB $ getBy $ UniqueArticleSlug slug
      case mArticle of

        Just (Entity articleId _) -> do
          runDB $ deleteBy $ UniqueArticleFavorite articleId userId
          getArticle articleId

        _  ->
          notFound

--------------------------------------------------------------------------------
-- Get all articles' tags

getTagsR :: Handler Value
getTagsR = do
  tags :: [Entity Tag] <- runDB $ selectList [] []
  return $ object ["tags" .= tags]

--------------------------------------------------------------------------------
-- Helpers

getArticle :: ArticleId -> Handler Value
getArticle articleId = do
  mCurrentUserId <- maybeAuthId
  mArticle <- Database.getArticle mCurrentUserId articleId
  case mArticle of
    Just article -> return $ object ["article" .= article]
    _            -> notFound

getComment :: ArticleCommentId -> Handler Value
getComment commentId = do
  mUserId <- maybeAuthId
  mComment <- Database.getComment mUserId  commentId
  case mComment of
    Just comment -> return $ object ["comment" .= comment]
    _            -> notFound

toSlug :: Text -> IO Text
toSlug text = do
  postfix <- liftIO randomSlugPostfix
  let slug = toSlug' text
  return $ slug <> T.pack ("-" ++ postfix)

toSlug' :: Text -> Text
toSlug' = take (maxSlugLength - slugPostfixLength - 1) . toLower . T.map h
  where
    h ' ' = '-'
    h c   = c

maxSlugLength :: Int
maxSlugLength = 255

slugPostfixLength :: Int
slugPostfixLength = 6

randomSlugPostfix :: IO String
randomSlugPostfix =
  take slugPostfixLength <$>
  generate (listOf $ elements $ ['0' .. '9'] ++ ['a' .. 'z'])
