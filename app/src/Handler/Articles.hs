{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.Articles
  ( getArticlesR
  , getArticlesFeedR
  , getArticleR
  , postArticlesR
  , putArticleR
  )
  where

import           Data.Aeson
import           Data.List                 ((!!))
import qualified Data.Text                 as T
import           Data.Text.Read            (decimal)
import           Database.Esqueleto        ((==.), (?.), (^.))
import qualified Database.Esqueleto        as E
import           Database.Persist.Extended (maybeUpdate)
import           Handler.Profiles          (encodeProfile)
import           Import                    hiding ((==.))
import           System.Random             (RandomGen, newStdGen, randomRs)
import           Web.Forma.Extra


--------------------------------------------------------------------------------
-- List Articles

getArticlesR :: Handler Value
getArticlesR = do
  mTag <- lookupGetParam "tag"
  mFilterAuthor <- lookupGetParam "author"
  mFilterFavoritedBy <- lookupGetParam "favorited"

  let
    filterTag article =
      case mTag of
        Just aTag ->
          E.exists $
          E.from $ \(tag `E.InnerJoin` articleTag) -> do
            E.on $ tag ^. TagId ==. articleTag ^. ArticleTagTag
            E.where_ $ articleTag ^. ArticleTagArticle ==. article ^. ArticleId
            E.where_ $ tag ^. TagName ==. E.val aTag
        _ -> E.val True

    filterAuthor author =
      case mFilterAuthor of
        Just username -> E.val username ==. author ^. UserUsername
        _             -> E.val True

    filterFavoritedBy =
      case mFilterFavoritedBy of
        Just favoritedBy ->
          E.exists $
          E.from $ \(favorite `E.InnerJoin` user) -> do
            E.on $ favorite ^. ArticleFavoriteUser ==. user ^. UserId
            E.where_ $ E.val favoritedBy ==. user ^. UserUsername
        _ -> E.val True

  articles <- getArticles $ \article author _ -> do
    E.where_ $ filterTag article
    E.where_ $ filterAuthor author
    E.where_ filterFavoritedBy

  return $ object ["articles" .= articles]

--------------------------------------------------------------------------------
-- Article feed

getArticlesFeedR :: Handler Value
getArticlesFeedR = do
  articles <- getArticles $ \_ _ following ->
    E.where_ following

  return $ object ["articles" .= articles]

--------------------------------------------------------------------------------
-- Get article

getArticleR :: Text -> Handler Value
getArticleR slug = do
  mArticle <- runDB $ getBy $ UniqueArticleSlug slug

  case mArticle of
    Just (Entity articleId _) -> encodeArticle articleId
    _            -> notFound

--------------------------------------------------------------------------------
-- Create article

type ArticleFields = '[ "article", "title", "description", "body", "tagList" ]

data CreateArticle = CreateArticle
  { createArticleTitle       :: Text
  , createArticleDescription :: Text
  , createArticleBody        :: Text
  , createArticleTagList     :: Maybe [Text]
  } deriving Show

createArticleForm :: FormParser ArticleFields Text Handler CreateArticle
createArticleForm =
  subParser #article (CreateArticle
    <$> field #title notEmpty
    <*> field #description notEmpty
    <*> field #body notEmpty
    <*> optional (field' #tagList))

postArticlesR :: Handler Value
postArticlesR = do
  Just userId <- maybeAuthId
  withForm createArticleForm $ \CreateArticle {..} -> do
    now <- liftIO getCurrentTime
    slug <- liftIO $ toSlug createArticleTitle
    let createdArticle =
          Article
            userId
            createArticleTitle
            slug
            createArticleDescription
            createArticleBody
            now
            now

    articleId <- runDB $ insert createdArticle

    _ <-
      forM_ createArticleTagList $
      mapM_ $ \tag -> do
        mTag <- runDB $ getBy $ UniqueTagName tag
        tagId <- case mTag of
            Just (Entity tagId _) -> return tagId
            _                     -> runDB $ insert $ Tag tag
        _ <- runDB $ insert $ ArticleTag articleId tagId
        return ()

    articles <- getArticles $ \article _ _ ->
      E.where_ $ article ^. ArticleId ==. E.val articleId

    case articles of
      []          -> notFound
      article : _ -> return $ object ["article" .= article]

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
  Just userId <- maybeAuthId
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
            ]
    runDB $ update articleId updates
    encodeArticle articleId

--------------------------------------------------------------------------------
-- Helpers

encodeArticle :: Key Article -> Handler Value
encodeArticle articleId = do
  articles <- getArticles $ \article _ _ ->
    E.where_ $ article ^. ArticleId ==. E.val articleId
  case articles of
    [] -> notFound
    article : _ -> return $ object ["article" .= article]

getArticles ::
  (E.SqlExpr (Entity Article)
    -> E.SqlExpr (Entity User)
    -> E.SqlExpr (E.Value Bool)
    -> E.SqlQuery ()
  )
  -> HandlerT App IO [Value]
getArticles extraQuery = do
  mCurrentUserId <- maybeAuthId
  mLimit <- lookupGetParam "limit"
  mOffset <- lookupGetParam "offset"

  let
    limit = decimalWithDefault mLimit 20
    offset = decimalWithDefault mOffset 0

    articleFavorites article =
      E.from $ \favorite -> do
        E.where_ $ favorite ^. ArticleFavoriteArticle ==. article ^. ArticleId
        return E.countRows

  articles <-
    runDB $
    E.select $
    E.from $ \(article
               `E.InnerJoin` author
               `E.LeftOuterJoin` mFollower
               `E.LeftOuterJoin` mFavourite) -> do
      let following = E.not_ $ E.isNothing $ mFollower ?. UserFollowerId
          favorited = E.not_ $ E.isNothing $ mFavourite ?. ArticleFavoriteId
          favoritesCount = E.sub_select $ articleFavorites article

      E.on $ article ^. ArticleAuthor ==. author ^. UserId
      E.on $ mFollower ?. UserFollowerUser ==. E.just (author ^. UserId)
      E.on $ mFollower ?. UserFollowerFollower ==. E.val mCurrentUserId
      E.on $ mFavourite ?. ArticleFavoriteUser ==. E.val mCurrentUserId
      E.limit limit
      E.offset offset
      E.orderBy [ E.desc $ article ^. ArticleCreatedAt ]
      extraQuery article author following

      return (article, author, following, favorited, favoritesCount)

  mapM addArticleTagList articles

addArticleTagList ::
     ( Entity Article, Entity User, E.Value Bool
     , E.Value Bool, E.Value Int
     )
  -> Handler Value
addArticleTagList
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

decimalWithDefault :: Integral p => Maybe Text -> p -> p
decimalWithDefault x default' =
        case decimal <$> x of
          Just (Right (l, _)) -> l
          _                   -> default'

toSlug :: Text -> IO Text
toSlug text = do
  unique <- liftIO $ alphaNum randomCharsLength
  let slug = toSlug' text
  return $ slug <> T.pack ("-" ++ unique)

toSlug' :: Text -> Text
toSlug' = take (maxSlugLength - randomCharsLength - 1) . toLower . T.map h
  where
    h ' ' = '-'
    h c   = c

maxSlugLength :: Int
maxSlugLength = 255

randomCharsLength :: Int
randomCharsLength = 6

alphaNum :: Int -> IO String
alphaNum n = take n . randomAlphaNum <$> newStdGen

randomAlphaNum :: RandomGen g => g -> String
randomAlphaNum = randomEl (['0'..'9'] ++ ['a'..'z'])

randomEl :: RandomGen g => [a] -> g -> [a]
randomEl xs g = (xs !!) <$> randomRs (0, length xs - 1) g

