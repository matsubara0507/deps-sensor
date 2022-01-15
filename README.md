# deps-sensor

Haskell Dependency Monitor:

Collect and display dependecies for Haskell applications at config.

## Build

```
$ elm make elm-src/Main.elm --output assets/main.js --optimize
$ stack build
```

### Bazel

```
$ bazelisk build //:deps-sensor-binary
```

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/deps-sensor . --build-arg local_bin_path=./bin
```

## Development

use Bazel

```
$ stack build --dry-run # compile hpack
$ bazelisk run //:gazelle
$ bazelisk run //:gazelle-update-repos
$ bazelisk test //...
```
