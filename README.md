# wai-session-redis

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/t4ccer/wai-session-redis/Build?label=build)](https://github.com/t4ccer/wai-session-redis/actions/workflows/build.yml)
[![Hackage](https://img.shields.io/hackage/v/wai-session-redis?color=blue)](https://hackage.haskell.org/package/wai-session-redis)

Provides Redis based session store for [Network.Wai.Session](https://hackage.haskell.org/package/wai-session)  
For example usage view [example/Main.hs](https://github.com/t4ccer/wai-session-redis/tree/main/example/Main.hs)

## Tests
To run tests `wai-session-redis` must have access to running `redis` instance.
### Using docker and stack
```bash
docker run --name redis-session-tests -p 6379:6379 -d redis
stack test
docker rm -f redis-session-tests
```

### Using nix
Nix takes care of running `redis` instance and testing `wai-session-redis`.
```bash
nix-build
```
