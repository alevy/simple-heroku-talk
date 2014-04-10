---
title: "Simple"
subtitle: a functional web framework in haskell
author:
  - amit levy (@aalevy)
date: April 11, 2014 @ Heroku
---

# Agenda

* Why write web applications in Haskell?
* A brief introduction to Haskell
* A look under the hood of Simple
* Walkthrough building a Simple app

# Why write web apps in Haskell?

# Why write (web apps in) Haskell?

  * Expressiveness
    * Small language core provides a lot of flexibility
    * Code can be very concise, speeding development
  * Correctness / safety / security / productivity
    * Types let you reason about **what** code is doing  
      even complicated code.
    * Eliminate whole classes of bugs (anecdotally, >90% of exceptions thrown
      in YouTube are type errors).
  * Perfomance
  * Joy

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

# A Brief Introduction to Haskell - Syntax



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

# Trying to do this procedurally

```ruby
def posts_controller(req)
  if req["PATH_INFO"].empty?
    return [200, [], Post.all.to_json]
  end

  post_id = req["PATH_INFO"].first
  return [200, [], Post.find(post_id)]
end

def myapp(req)
  if req["PATH_INFO"].first == "posts"
    req["PATH_INFO"].shift
    if resp = posts_controller(req)
      return resp
    end
    req["PATH_INFO"].unshift("posts")
  end

  if req["PATH_INFO"].first == "users"
    req["PATH_INFO"].shift
    if resp = users_controller(req)
      return resp
    end
    req["PATH_INFO"].unshift("users")
  end

  if resp = posts_controller(req)
    return resp
  end
  return [404, [["Content-Length", 0]], ""]
end
```

# Trying to do this procedurally - OK, we need helpers

`route_name` will call pass control to a block if it matches the first path
directory:

```ruby
def route_name(req, path_name, &sub_route)
  pn = req["PATH_INFO"].shift
  if pn == path_name
    if resp = sub_route.call(req)
      return resp
    end
  end
  req["PATH_INFO"].unshift(pn)
  return nil
end
```

which let's us write:

```ruby
def myapp(req)
  resp = route_name("posts") do |req|
    resp = route_top {|_req| return [200, [], Post.all.to_json]}
    return resp || [200, [], Post.find(req["POST_INFO"].first)]
  end
  return resp if resp

  resp = route_name("users") do |req|
    ...
  end
  return resp
end
```

better, but we still have to check the return value everywhere...

can you imagine writing actual code like this?

# Continuation-passing-style (CPS) in JavaScript

You're already familiar with CPS if you've written in Node.js:

```javascript
function route_name(req, name, match_callback, fallback) {
  if (req.path_info[0] == name) {
    req.path_info.shift();
    return match_callback(req, function() {
      req.path_info.unshift(name);
      return fallback(req);
    });
  } else {
    return fallback(req);
  }
}
```

# CPS in JavaScript

So then we get to write:

```javascript
function myapp(req) {
  return route_name("posts",
    function(req, fallback) {
      return route_top(function() {
        return [200, [], get_all_posts()];
      }, function(req) {
        return [200, [], get_post(req.path[0])];
      });
    },
    function(req) {
      return route_name("users",
        function(req) {
          ...
        },
        function(req) {
          ...
        });
    });
}
```

# CPS in JavaScript

OK, and we can even refactor a bit:

```javascript
function postsController(req) {
  return route_top(function() {
    return [200, [], get_all_posts()];
  }, function(req) {
    return [200, [], get_post(req.path[0])];
  });
}

function usersController(req, fallback) { ... }

function myapp(req) {
  return route_name("posts", postsController,
    function(req) {
      return route_name("users", usersController
          postsController);
    });
}
```

* But this is pretty ugly, and not really what we want

* Also pretty sure it's not correct...

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

