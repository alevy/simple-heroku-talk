---
title: "Simple"
subtitle: a functional web framework in haskell
author:
  - amit levy (@aalevy)
date: April 11, 2014 @ Heroku
---

# Haskell in a nutshell

# A Teaser

```haskell
get "/" $ do
  posts <- withConnection $ \conn ->
    liftIO $ findAll conn
  render "index.html" $
    object ["posts" .= (posts :: [Post])]

get "/:post_id" $ do
  postId <- queryParam' "post_id"
  mpost <- withConnection $ \conn ->
    liftIO $ findRow postId
  case mpost of
    Nothing -> respond notFound
    Just post ->
      render "show.html" $
        object ["post" .= post :: Post]
  render "show.html" post
```

# A Web Framework in Four Lines

```haskell
type ControllerState s = (Request, s)

newtype Controller s a = {
  runController :: ControllerState s
    -> IO (Either Response a, ControllerState s)  
}
```

# A Web Framework in Four Lines

```haskell
ControllerState s
  -> IO (Either Response a, ControllerState s)  
```

A function that takes a Request value and the current app state, and returns
either:

1. A `Response` value -- meaning we're done

2. A value of type `a` -- this lets us do useful work in addition to just
  responding to a Request.

# A Web Framework in Four Lines

Let's write a utility functions:

```haskell
respond :: Response -> Controller s ()
```

Given a `Response`, construct a `Controller` action that
responds with it:

```haskell
respond resp = \s -> return (Left resp, s)
```

# A Web Framework in Four Lines

Let's write a utility functions:

```haskell
return :: a -> Controller s a
```

Given a value (of arbitrary type `a`), _lift_ it into the Controller monad:

```haskell
return val = \s -> return (Right val, s)
```

