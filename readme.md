# ![RealWorld Example App](logo.png) (WIP)

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
- [ ] Get Profile (`GET/api/profiles/:username`)
- [ ] Follow user (`POST /api/profiles/:username/follow`)
- [ ] Unfollow user (`DELETE /api/profiles/:username/follow`)
- [ ] List Articles (`GET /api/articles`)
- [ ] Feed Articles (`GET /api/articles/feed`)
- [ ] Get Article (`GET /api/articles/:slug`)
- [ ] Create Article (`POST /api/articles`)
- [ ] Update Article (`PUT /api/articles/:slug`)
- [ ] Delete Article (`DELETE /api/articles/:slug`)
- [ ] Add Comments to an Article (`POST /api/articles/:slug/comments`)
- [ ] Get Comments from an Article (`GET /api/articles/:slug/comments`)
- [ ] Delete Comment (`DELETE /api/articles/:slug/comments/:id`)
- [ ] Favourite Article (`POST /api/articles/:slug/favorite`)
- [ ] Unfavourite Article (`DELETE /api/articles/:slug/favorite`)
- [ ] Get Tags (`GET /api/tags`)

# How it works

> Describe the general architecture of your app here

# Getting started

> npm install, npm start, etc.
