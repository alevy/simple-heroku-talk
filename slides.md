---
title: Simple
subtitle: a functional web framework in haskell
author:
  - amit levy (@aalevy)
date: April 11, 2014 @ Heroku
css: style.css
---

# `$ whoami`

![Grad student at Stanford](scs.png)
![Work on MemCachier](memcachier.png)

* Graduate student in the Secure Computer Systems lab at Stanford
* Work on MemCachier

# A Typical Rails Learning Curve

![](learning-curve.svg "not to scale")

# What we really want...

![](what-we-want.svg "not to scale")

# What we really want...

  * Intuitive API
    - easy to get started
    - readable application code

  * Easy to understand internals
    - hackable
    - coherence between what I write and what gets executed

  * Expose functionality to users
    - let app developer leverage core functionality

# _Simple_ - a "framework-less" web framework 

  * No prescribed app structure enforced
    * but sane default conventions
  * Small core
    * <400 lines of code
    * can understand most of it in one source file
  * Non-core broken up into composable components
    * no special access to core primitives
    * anyone can build alternative/additional components
  * Fully-loaded:
    * Request routing
    * View templates
    * Cookie-based sessions
    * ORM for PostgreSQL

# Agenda

  * Why write web applications in Haskell?
  * A brief introduction to Haskell
  * A look under the hood of Simple
  * Walkthrough building a Simple app

# Why write web apps in Haskell?

# Why write (web apps in) Haskell?

  * Expressiveness
    * Small language core provides a lot of flexibility
    * Code can be very concise
  * Correctness / safety / security / productivity
    * Types let you reason about **what** code is doing  
      even complicated code.
    * Eliminates a whole class of bugs (anecdotally, >90% of exceptions thrown
      in YouTube are type errors).
  * Perfomance

# Throughput (req/sec) compared to Ruby, node.js

![](performance.svg)

_This was done informally on my laptop, but more robust benchmarks have similar
results_

# A Brief Introduction to Haskell

  * Purely-functional
    * Expressions vs. Statements
    * First-class functions
    * Partial application
  * Strict type-system
    * Types known at compile time
    * Side effects are explicit
  * Advanced tools
    * Concurrency + M/N parallelism built in
    * Testing frameworks
    * `cabal` package manager
    * ...

# Hello World

```haskell
main = putStrLn "hello world"
```

# Hello World

```haskell
main = putStrLn "hello world"
```

Let's get a bit fancier:

```haskell
main = do
  putStr "What's your name? "
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"
```

> * Each statement in the `do` block returns a value (consumed with `<-`) and
    can perform a side effect (e.g. print to the screen).

> * The `do` block let's us write statements that have side effects.

> * `main` is the root of all side effects in a program

> * The do block lets us program what side effects can happen
    * hide state
    * restrict actions by a security policy
    * provide transactional memory

# A Brief Introduction to Haskell - Primitive Types

  * `Bool`, `Char`, `Int`, `Double`...
```haskell
let amMutable = False
```
  * *type1* `->` *type2* - a function from *type1* to *type2*. For example:
```haskell
add :: Int -> Int -> Int
map :: (a -> b) -> [a] -> [b]
```
  * `(`*type1*`,` *type2*`,` ...`,` *typeN*`)` - a tuple
  * `()` - a zero-tuple, pronounced *unit*
    * kind of like `void` in C
    * there is only one value of this type, also written `()`

# A Brief Introduction to Haskell - User-defined types

* `data` or `newtype`

```haskell
data HTMLString = UnescapedHTML String | EscapedHTML String

data Maybe a = Just a | Nothing

data Either a b = Left a | Right b

data BlogPost = BlogPost { postTitle :: String, postBody :: String }

newtype Password = Password { passwordDigest :: String }
```

* `type` aliases

```haskell
type Point = (Double, Double)
```

# Controller code for a Blog

```haskell
main = run 3000 $ do
  settings <- newAppSettings
  controllerApp settings $ do
    get "/" $ render "welcome.html" ()

    routeName "/posts" $ do
      get "/" $ do
        posts <- findAllPosts
        render "posts/index.html" posts

      get "/:post_id" $ do
        postId <- readQueryParam' "post_id"
        routePost postId $ \post ->
            render "posts/show.html" post

      get "/:tag" $ do
        tag <- queryParam' "tag"
        posts <- findPostsByTag tag
        render "posts/index.html" posts

routePost :: DBRef Post -> (Post -> Controller BlogSettings ())
         -> Controller BlogSettings ()
routePost postId act = withConnection $ \conn -> do
  mpost <- liftIO $ findRow conn postId
  maybe (return ()) act mpost
```

# A Web Framework in Four Lines

```haskell
type ControllerState s = (Request, s)

newtype Controller s a = Controller {
  runController :: ControllerState s
                -> IO (Either Response a, ControllerState s)
}
```

> * A function that takes a `Request` and an app specific state
> * Might generated a response, otherwise returns a value
> * The `Request` or app state might be modified
> * `IO` in the return type means we can perform `IO` side-effects

# Some building blocks to make life easier

Given a `Response`, construct a `Controller` action that
responds with it:

```haskell
respond :: Response -> Controller s ()
respond resp = Controller $ \s -> return (Left resp, s)
```

Given a value (of arbitrary type `a`), return it rather than responding:
```haskell
return :: a -> Controller s a
return val = Controller $ \s -> return (Right val, s)
```

Get the `Request` value:

```haskell
request :: Controller s Request
request = Controller $ \s -> return (Right $ fst s, s)
```

These are provided by _Simple_ but don't do anything special. Just used
everywhere.

# Let's write a routing combinator

Remember `routeName` from the example?

```
...
routeName "posts" $ do
  ...
...
```

If the first thing in the `Request` path `==` the name, match this route,
otherwise try the next action:

```haskell
routeName :: Text -> Controller s () -> Controller s ()
routeName name (Controller next) = do
  req <- request
  if (length $ pathInfo req) > 0 && name == (head $ pathInfo req)
    then Controller $ \(req, s) -> next (popHdr req, s)
    else return ()
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }
```

> * Easy to see how to you would write `routeHost`, `routeMethod`,
  `routeAccept`...

> * All these are provided but you can write your own (e.g. `routePost`).

# OK, enough chatter

Let's write a blog...

# Thanks!

```bash
$ cabal install simple
$ smpl create test_app
$ cd test_app
$ cabal install --only-dependencies
$ cabal run
```

* [http://simple.cx](http://simple.cx)
* [https://github.com/alevy/simple](https://github.com/alevy/simple)
* [http://hackage.haskell.org/package/simple](http://hackage.haskell.org/package/simple)
