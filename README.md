# wai-session-redis
Provides Redis based session store for [Network.Wai.Session](https://hackage.haskell.org/package/wai-session)  
For example usage view [example/Main.hs](example/Main.hs)

## Tests
To run tests `wai-session-redis` must have access to running `redis` instance.
### Using docker
```bash
docker run --name redis-session-tests -p 6379:6379 -d redis
stack test
docker rm -f redis-session-tests
```
