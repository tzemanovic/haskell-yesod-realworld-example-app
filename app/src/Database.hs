{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database
  ( ArticleData(..)
  , CommentData(..)
  -- * Queries
  , getArticle
  , getGlobalArticleFeed
  , getUserArticleFeed
  , getComment
  , getCommentsByArticleSlug
  ) where

import           ClassyPrelude.Yesod hiding (Value, isNothing, on, (==.), (||.))
import           Database.Esqueleto
import           Foundation
import           Handler.Profiles    (encodeProfile)
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

--------------------------------------------------------------------------------
-- Queries

getArticle ::
  Maybe (Key User)
  -> Key Article
  -> Handler (Maybe ArticleData)
getArticle mCurrentUserId articleId = do
  articles <- getArticles mCurrentUserId $ \article _ _ ->
    where_ $ article ^. ArticleId ==. val articleId
  return $ head <$> fromNullable articles

getGlobalArticleFeed ::
  Maybe (Key User)
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

    filterFavoritedBy =
      case mFavoritedBy of
        Just favoritedBy ->
          exists $
          from $ \(favorite `InnerJoin` user) -> do
            on $ favorite ^. ArticleFavoriteUser ==. user ^. UserId
            where_ $ val favoritedBy ==. user ^. UserUsername
        _ -> val True

    clause article author _ = do
      where_ $ filterTag article
      where_ $ filterAuthor author
      where_ filterFavoritedBy

  paginateArticles
    (getArticles mCurrentUserId)
    (getArticlesCount mCurrentUserId)
    page
    clause

getUserArticleFeed ::
  Maybe (Key User)
  -> Page
  -> Handler ([ArticleData], Int)
getUserArticleFeed mCurrentUserId page = do
  let
    clause _ _ following =
      where_ following

  paginateArticles
    (getArticles mCurrentUserId)
    (getArticlesCount mCurrentUserId)
    page
    clause

getComment ::
  Maybe (Key User)
  -> Key ArticleComment
  -> Handler (Maybe CommentData)
getComment mCurrentUserId commentId = do
  comments <- Database.getComments mCurrentUserId $ \comment _ ->
    where_ $ comment ^. ArticleCommentId ==. val commentId
  return $ head <$> fromNullable comments

getCommentsByArticleSlug ::
  Maybe (Key User)
  -> Text
  -> Handler [CommentData]
getCommentsByArticleSlug mCurrentUserId slug =
  getComments mCurrentUserId $ \_ article ->
    where_ $ article ^. ArticleSlug ==. val slug

--------------------------------------------------------------------------------
-- Helpers

type ArticleClause
   = SqlExpr (Entity Article)
     -> SqlExpr (Entity User)
     -> SqlExpr (Value Bool)
     -> SqlQuery ()

paginateArticles :: Monad m
  => (ArticleClause -> m articles)
  -> (ArticleClause -> m [Value articlesCount])
  -> Page
  -> ArticleClause
  -> m (articles, articlesCount)
paginateArticles query countQuery page clause = do
  [Value articleCount] <- countQuery clause
  articles <-
    query $ \article author following -> do
      clause article author following
      paginate page
  return (articles, articleCount)

getArticlesCount ::
  Maybe (Key User)
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

getArticles ::
  Maybe (Key User)
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

getComments ::
  Maybe (Key User)
  -> (SqlExpr (Entity ArticleComment)
       -> SqlExpr (Entity Article)
       -> SqlQuery ()
     )
  -> Handler [CommentData]
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

      on $ mFollower ?. UserFollowerUser ==. just (author ^. UserId)
      on $ author ^. UserId ==. comment ^. ArticleCommentAuthor
      on $ article ^. ArticleId ==. comment ^. ArticleCommentArticle
      where_ $
        mFollower ?. UserFollowerFollower ==. val mCurrentUserId ||.
        isNothing (mFollower ?. UserFollowerId)
      extraClause comment article
      return (comment, author, following)

  return $ CommentData <$> comments

getArticleTags :: Key Article -> Handler [Value Text]
getArticleTags articleId =
    runDB $
    select $
    from $ \(articleTag `InnerJoin` tag) -> do
      where_ $ articleTag ^. ArticleTagArticle ==. val articleId
      where_ $ tag ^. TagId ==. articleTag ^. ArticleTagTag
      return $ tag ^. TagName
