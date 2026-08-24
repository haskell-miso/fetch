-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import           Miso.JSON          hiding ((.=))
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onInput, onSubmit)
import qualified Miso.Html.Property as P
import           Miso.String        (null, strip)
import           Prelude            hiding (null)
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | A GitHub user (or organization).
data GitHubUser = GitHubUser
  { userLogin     :: MisoString
  , userName      :: Maybe MisoString
  , userAvatar    :: MisoString
  , userBio       :: Maybe MisoString
  , userFollowers :: Int
  , userRepoCount :: Int
  , userUrl       :: MisoString
  } deriving (Eq, Show)
----------------------------------------------------------------------------
instance FromJSON GitHubUser where
  parseJSON = withObject "user" $ \o -> GitHubUser
    <$> o .:  "login"
    <*> o .:? "name"
    <*> o .:  "avatar_url"
    <*> o .:? "bio"
    <*> o .:  "followers"
    <*> o .:  "public_repos"
    <*> o .:  "html_url"
----------------------------------------------------------------------------
-- | A repository, as returned by the @/users/:login/repos@ endpoint.
data GitHubRepo = GitHubRepo
  { repoName  :: MisoString
  , repoDesc  :: Maybe MisoString
  , repoStars :: Int
  , repoLang  :: Maybe MisoString
  , repoUrl   :: MisoString
  } deriving (Eq, Show)
----------------------------------------------------------------------------
instance FromJSON GitHubRepo where
  parseJSON = withObject "repo" $ \o -> GitHubRepo
    <$> o .:  "name"
    <*> o .:? "description"
    <*> o .:  "stargazers_count"
    <*> o .:? "language"
    <*> o .:  "html_url"
----------------------------------------------------------------------------
data Model = Model
  { _query :: MisoString
  , _user  :: Maybe GitHubUser
  , _repos :: [GitHubRepo]
  , _busy  :: Bool
  , _oops  :: Maybe MisoString
  } deriving (Eq, Show)
----------------------------------------------------------------------------
query :: Lens Model MisoString
query = lens _query $ \m x -> m { _query = x }

user :: Lens Model (Maybe GitHubUser)
user = lens _user $ \m x -> m { _user = x }

repos :: Lens Model [GitHubRepo]
repos = lens _repos $ \m x -> m { _repos = x }

busy :: Lens Model Bool
busy = lens _busy $ \m x -> m { _busy = x }

oops :: Lens Model (Maybe MisoString)
oops = lens _oops $ \m x -> m { _oops = x }
----------------------------------------------------------------------------
data Action
  = SetQuery MisoString
  | Search
  | GotUser (Response GitHubUser)
  | GotRepos (Response [GitHubRepo])
  | FetchFailed (Response MisoString)
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model "haskell-miso" Nothing [] False Nothing
----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { mount = Just Search
  }
----------------------------------------------------------------------------
api :: MisoString
api = "https://api.github.com/users/"
----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  SetQuery q ->
    query .= q
  Search -> do
    q <- strip <$> use query
    if null q
      then pure ()
      else do
        busy .= True
        oops .= Nothing
        getJSON (api <> q) [] GotUser FetchFailed
  GotUser Response {..} -> do
    user ?= body
    getJSON (api <> userLogin body <> "/repos?sort=updated&per_page=6")
      [] GotRepos FetchFailed
  GotRepos Response {..} -> do
    busy .= False
    repos .= body
  FetchFailed Response {..} -> do
    busy .= False
    user .= Nothing
    repos .= []
    oops ?= case status of
      Just 404 -> "No such user or organization."
      Just 403 -> "GitHub API rate limit reached — try again in a minute."
      _        -> "Request failed: " <> body
----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 🌐 ", H.a_ [ P.href_ repoLink ] [ "miso-fetch" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "The browser Fetch API from Haskell: typed JSON requests against "
      , "the live GitHub API, decoded with "
      , H.code_ [] [ "Miso.JSON" ]
      , "."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoLink ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "panel" ]
    ( [ H.form_
        [ P.class_ "search", onSubmit Search ]
        [ H.input_
          [ P.type_ "text"
          , P.value_ (m ^. query)
          , P.placeholder_ "GitHub user or organization…"
          , P.autofocus_ True
          , onInput SetQuery
          ]
        , H.button_
          [ P.class_ "btn", P.type_ "submit" ]
          [ text (if m ^. busy then "Fetching…" else "Fetch") ]
        ]
      ]
      ++ [ H.p_ [ P.class_ "error" ] [ text e ] | Just e <- [ m ^. oops ] ]
      ++ maybe [] (pure . userCard) (m ^. user)
      ++ [ repoGrid (m ^. repos) | m ^. repos /= [] ]
    )
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly. Requests go "
      , "straight to ", H.code_ [] [ "api.github.com" ], "; no backend involved."
      ]
    ]
  ]
  where
    repoLink = "https://github.com/haskell-miso/miso-fetch"
----------------------------------------------------------------------------
userCard :: GitHubUser -> View () Model Action
userCard GitHubUser {..} =
  H.section_
  [ P.class_ "user" ]
  [ H.img_ [ P.class_ "avatar", P.src_ userAvatar, P.alt_ (userLogin <> " avatar") ]
  , H.div_
    [ P.class_ "user-info" ]
    ( [ H.h2_ []
        [ H.a_ [ P.href_ userUrl ] [ text (maybe userLogin id userName) ]
        , H.span_ [ P.class_ "login" ] [ text ("@" <> userLogin) ]
        ]
      ]
      ++ [ H.p_ [ P.class_ "bio" ] [ text b ] | Just b <- [ userBio ] ]
      ++ [ H.p_ [ P.class_ "stats" ]
           [ H.strong_ [] [ text (ms userRepoCount) ], " public repos · "
           , H.strong_ [] [ text (ms userFollowers) ], " followers"
           ]
         ]
    )
  ]
----------------------------------------------------------------------------
repoGrid :: [GitHubRepo] -> View () Model Action
repoGrid rs =
  H.div_
  []
  [ H.h3_ [ P.class_ "repos-title" ] [ "Recently updated repositories" ]
  , H.div_
    [ P.class_ "repos" ]
    [ H.a_
      [ P.class_ "repo", P.href_ repoUrl ]
      ( [ H.h4_ [] [ text repoName ] ]
        ++ [ H.p_ [ P.class_ "desc" ] [ text d ] | Just d <- [ repoDesc ] ]
        ++ [ H.p_ [ P.class_ "meta" ]
             [ text ("★ " <> ms repoStars)
             , text (maybe "" (" · " <>) repoLang)
             ]
           ]
      )
    | GitHubRepo {..} <- rs
    ]
  ]
----------------------------------------------------------------------------
