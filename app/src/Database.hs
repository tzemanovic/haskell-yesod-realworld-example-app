{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database
  ( ArticleData(..)
  , CommentData(..)
  , encodeProfile
  -- * Queries
  , getArticle
  , getGlobalArticleFeed
  , getUserArticleFeed
  , getComment
  , getCommentsByArticleSlug
  ) where

import           ClassyPrelude.Yesod hiding (Value, isNothing, on, (==.))
import qualified Data.Aeson          as JSON
import           Database.Esqueleto
import           Foundation
import           Model
import           Pagination


--------------------------------------------------------------------------------
-- Data

data ArticleData = ArticleData (Entity Article)
                               (Entity User)
                               [Value Text]
                               (Value Bool)
                               (Value Bool)
                               (Value Int)

instance ToJSON ArticleData where
  toJSON (ArticleData (Entity _ Article {..})
                      (Entity _ author)
                      tags
                      (Value following)
                      (Value favorited)
                      (Value favoritesCount)) =
    object
      [ "slug" .= articleSlug
      , "title" .= articleTitle
      , "description" .= articleDescription
      , "body" .= articleBody
      , "tagList" .= (unValue <$> tags :: [Text])
      , "createdAt" .= articleCreatedAt
      , "updatedAt" .= articleUpdatedAt
      , "favorited" .= favorited
      , "favoritesCount" .= favoritesCount
      , "author" .= encodeProfile author following
      ]

newtype CommentData = CommentData ( Entity ArticleComment
                                  , Entity User
                                  , Value Bool
                                  )

instance ToJSON CommentData where
  toJSON (CommentData ( Entity commentId ArticleComment {..}
                      , Entity _ author
                      , Value following
                      )) =
    object
      [ "id" .= commentId
      , "createdAt" .= articleCommentCreatedAt
      , "updatedAt" .= articleCommentUpdatedAt
      , "body" .= articleCommentBody
      , "author" .= encodeProfile author following
      ]

encodeProfile :: User -> Bool -> JSON.Value
encodeProfile User {..} following =
  object
    [ "username" .= userUsername
    , "bio" .= userBio
    , "image" .= userImage
    , "following" .= following
    ]

--------------------------------------------------------------------------------
-- Queries

-- | Get article by ID.
getArticle ::
  Maybe UserId
  -> ArticleId
  -> Handler (Maybe ArticleData)
getArticle mCurrentUserId articleId = do
  articles <- getArticles mCurrentUserId $ \article _ _ ->
    where_ $ article ^. ArticleId ==. val articleId
  return $ head <$> fromNullable articles

-- | Get articles by tag, author or favorited by with pagination.
getGlobalArticleFeed ::
  Maybe UserId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Page
  -> Handler ([ArticleData], Int)
getGlobalArticleFeed mCurrentUserId mTag mAuthor mFavoritedBy page = do
  let
    filterTag article =
      case mTag of
        Just aTag ->
          exists $
          from $ \(tag `InnerJoin` articleTag) -> do
            on $ tag ^. TagId ==. articleTag ^. ArticleTagTag
            where_ $ articleTag ^. ArticleTagArticle ==. article ^. ArticleId
            where_ $ tag ^. TagName ==. val aTag
        _ -> val True

    filterAuthor author =
      case mAuthor of
        Just username -> val username ==. author ^. UserUsername
        _             -> val True

    filterFavoritedBy article =
      case mFavoritedBy of
        Just favoritedBy ->
          exists $
          from $ \(favorite `InnerJoin` user) -> do
            on $ favorite ^. ArticleFavoriteUser ==. user ^. UserId &&.
                 favorite ^. ArticleFavoriteArticle ==. article ^. ArticleId
            where_ $ val favoritedBy ==. user ^. UserUsername
        _ -> val True

    clause article author _ = do
      where_ $ filterTag article
      where_ $ filterAuthor author
      where_ $ filterFavoritedBy article

  paginateArticles
    (getArticles mCurrentUserId)
    (getArticlesCount mCurrentUserId)
    page
    clause

-- | Get user's article feed by tag, author or favorited by with pagination.
getUserArticleFeed ::
  Maybe UserId
  -> Page
  -> Handler ([ArticleData], Int)
getUserArticleFeed userId page = do
  let
    clause _ _ following =
      where_ following

  paginateArticles
    (getArticles userId)
    (getArticlesCount userId)
    page
    clause

-- | Get article's comment by ID.
getComment ::
  Maybe UserId
  -> ArticleCommentId
  -> Handler (Maybe CommentData)
getComment mCurrentUserId commentId = do
  comments <- Database.getComments mCurrentUserId $ \comment _ ->
    where_ $ comment ^. ArticleCommentId ==. val commentId
  return $ head <$> fromNullable comments

-- | Get article's comments by article's slug.
getCommentsByArticleSlug ::
  Maybe UserId
  -> Text
  -> Handler [CommentData]
getCommentsByArticleSlug mCurrentUserId slug =
  getComments mCurrentUserId $ \_ article ->
    where_ $ article ^. ArticleSlug ==. val slug

--------------------------------------------------------------------------------
-- Helpers

type ArticleClause
   = SqlExpr (Entity Article) -- ^ article
     -> SqlExpr (Entity User) -- ^ article's author
     -> SqlExpr (Value Bool)  -- ^ is current user following author
     -> SqlQuery ()

-- | Paginate articles produced by a given query.
paginateArticles :: Monad m
  => (ArticleClause -> m [article])             -- ^ articles query
  -> (ArticleClause -> m [Value Int])           -- ^ count query
  -> Page                                       -- ^ page settings
  -> ArticleClause                              -- ^ clause used in both queries
  -> m ([article], Int)
paginateArticles query countQuery page clause = do
  aCount <- countQuery clause
  articles <-
    query $ \article author following -> do
      clause article author following
      paginate page
  case aCount of
    [Value articleCount] ->
      return (articles, articleCount)
    _ ->
      return (articles, length articles)

-- | Get articles count for a given clause.
getArticlesCount ::
  Maybe UserId
  -> ArticleClause
  -> Handler [Value Int]
getArticlesCount mCurrentUserId extraClause =
  runDB $
  select $
  from $ \(article
          `InnerJoin` author
          `LeftOuterJoin` mFollower
          ) -> do
    let following = not_ $ isNothing $ mFollower ?. UserFollowerId

    on $ mFollower ?. UserFollowerUser ==. just (author ^. UserId) &&.
         mFollower ?. UserFollowerFollower ==. val mCurrentUserId
    on $ article ^. ArticleAuthor ==. author ^. UserId
    orderBy [ desc $ article ^. ArticleCreatedAt ]
    extraClause article author following
    return countRows

-- | Get articles for a given clause.
getArticles ::
  Maybe UserId
  -> ArticleClause
  -> Handler [ArticleData]
getArticles mCurrentUserId extraClause = do
  let
    articleFavorites article =
      from $ \favorite -> do
        where_ $ favorite ^. ArticleFavoriteArticle ==. article ^. ArticleId
        return countRows

    addTags (article@(Entity articleId _), author, following, favorited,
       favoritesCount) = do
       tags <- getArticleTags articleId
       return $
         ArticleData article author tags following favorited favoritesCount

  articles <-
    runDB $
    select $
    from $ \(article
            `InnerJoin` author
            `LeftOuterJoin` mFollower
            `LeftOuterJoin` mFavourite
            ) -> do
      let following = not_ $ isNothing $ mFollower ?. UserFollowerId
          favorited = not_ $ isNothing $ mFavourite ?. ArticleFavoriteId
          favoritesCount = sub_select $ articleFavorites article

      on $ mFavourite ?. ArticleFavoriteUser ==. val mCurrentUserId
      on $ mFollower ?. UserFollowerUser ==. just (author ^. UserId) &&.
           mFollower ?. UserFollowerFollower ==. val mCurrentUserId
      on $ article ^. ArticleAuthor ==. author ^. UserId
      orderBy [ desc $ article ^. ArticleCreatedAt ]
      extraClause article author following
      return (article, author, following, favorited, favoritesCount)

  mapM addTags articles

type CommentClause
   = SqlExpr (Entity ArticleComment) -- ^ comment
      -> SqlExpr (Entity Article)    -- ^ article this comment belongs to
      -> SqlQuery ()

-- | Get comments for a given clause.
getComments :: Maybe UserId -> CommentClause -> Handler [CommentData]
getComments mCurrentUserId extraClause = do
  comments <-
    runDB $
    select $
    from $ \(comment
            `InnerJoin` article
            `InnerJoin` author
            `LeftOuterJoin` mFollower
            ) -> do
      let following = not_ $ isNothing $ mFollower ?. UserFollowerId

      on $ mFollower ?. UserFollowerUser ==. just (author ^. UserId) &&.
           mFollower ?. UserFollowerFollower ==. val mCurrentUserId
      on $ author ^. UserId ==. comment ^. ArticleCommentAuthor
      on $ article ^. ArticleId ==. comment ^. ArticleCommentArticle
      extraClause comment article
      return (comment, author, following)

  return $ CommentData <$> comments

-- | Get article tags by article ID.
getArticleTags :: ArticleId -> Handler [Value Text]
getArticleTags articleId =
    runDB $
    select $
    from $ \(articleTag `InnerJoin` tag) -> do
      where_ $ articleTag ^. ArticleTagArticle ==. val articleId
      where_ $ tag ^. TagId ==. articleTag ^. ArticleTagTag
      return $ tag ^. TagName
