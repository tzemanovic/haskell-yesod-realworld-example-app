# ![RealWorld Example App](logo.png) (WIP)

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org) [![Build Status](https://travis-ci.org/tzemanovic/haskell-yesod-realworld-example-app.svg?branch=dev)](https://travis-ci.org/tzemanovic/haskell-yesod-realworld-example-app)

> ### Haskell/Yesod codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://github.com/gothinkster/realworld)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with **Haskell/Yesod** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell/Yesod** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# Progress
API route status
- [X] Authentication (`POST /api/users/login`)
- [X] Registration (`POST /api/users`)
- [X] Get Current User (`GET /api/user`)
- [X] Update User (`PUT /api/user`)
- [X] Get Profile (`GET/api/profiles/:username`)
- [X] Follow user (`POST /api/profiles/:username/follow`)
- [X] Unfollow user (`DELETE /api/profiles/:username/follow`)
- [X] List Articles (`GET /api/articles`)
- [X] Feed Articles (`GET /api/articles/feed`)
- [X] Get Article (`GET /api/articles/:slug`)
- [X] Create Article (`POST /api/articles`)
- [X] Update Article (`PUT /api/articles/:slug`)
- [X] Delete Article (`DELETE /api/articles/:slug`)
- [X] Add Comments to an Article (`POST /api/articles/:slug/comments`)
- [X] Get Comments from an Article (`GET /api/articles/:slug/comments`)
- [ ] Delete Comment (`DELETE /api/articles/:slug/comments/:id`)
- [ ] Favourite Article (`POST /api/articles/:slug/favorite`)
- [ ] Unfavourite Article (`DELETE /api/articles/:slug/favorite`)
- [ ] Get Tags (`GET /api/tags`)

# How it works

> Describe the general architecture of your app here

# Getting started

> npm install, npm start, etc.
