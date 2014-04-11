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

# JavaScript

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

# JavaScript

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

# JavaScript

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

