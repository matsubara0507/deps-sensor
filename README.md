# deps-sensor

## Requirement

## Usage

## Build

```
$ elm make elm-src/Main.elm --output assets/main.js --optimize
$ stack build
```

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/deps-sensor . --build-arg local_bin_path=./bin
```
