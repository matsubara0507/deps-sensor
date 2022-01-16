workspace(name = "deps-sencor")

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "rules_haskell",
    sha256 = "851e16edc7c33b977649d66f2f587071dde178a6e5bcfeca5fe9ebbe81924334",
    strip_prefix = "rules_haskell-0.14",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.14.tar.gz",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)
load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    local_snapshot = "//:stack-snapshot.yaml",
    packages = [
        "aeson",  # keep
        "base",
        "dotenv",
        "extensible",
        "fallible",
        "file-embed",
        "githash",
        "github",
        "memory",
        "mix",
        "mix-plugin-github",
        "mix-plugin-shell",
        "optparse-applicative",  # keep
        "path",  # keep
        "path-io",  # keep
        "rio",
        "shelly",
        "tasty",
        "tasty-hspec",
        "yaml",
    ],
)

rules_haskell_toolchains(version = "8.10.7")

# for Gazelle
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "2b1641428dff9018f9e85c0384f03ec6c10660d935b750e3fa1492a281a53b0f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "de69a09dc70417580aabf20a28619bb3ef60d038470c7cf8442fafcf627c21cb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()

go_register_toolchains(version = "1.17.2")

gazelle_dependencies()

http_archive(
    name = "io_tweag_gazelle_cabal",
    sha256 = "103f915fce9add80eae820b00ae7d16e5557b944f110987d3b9b5940676c7e81",
    strip_prefix = "gazelle_cabal-main",
    url = "https://github.com/tweag/gazelle_cabal/archive/main.zip",
)

load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

gazelle_cabal_dependencies()

# for Elm
http_archive(
    name = "rules_elm",
    sha256 = "a9db7f55e3693ab94a60cbf602221095514aec6541253b21cc89f0ba1365d87c",
    urls = ["https://github.com/matsubara0507/rules_elm/releases/download/v1.0.0/rules_elm-v1.0.0.zip"],
)

load("@rules_elm//elm:repositories.bzl", rules_elm_repositories = "repositories")

rules_elm_repositories()

load("@rules_elm//elm:toolchain.bzl", rules_elm_toolchains = "toolchains")

rules_elm_toolchains(version = "0.19.1")

# for Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "59536e6ae64359b716ba9c46c39183403b01eabfbd57578e84398b4829ca499a",
    strip_prefix = "rules_docker-0.22.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.22.0/rules_docker-v0.22.0.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)
container_repositories()

load(
    "@io_bazel_rules_docker//repositories:deps.bzl",
    container_deps = "deps",
)
container_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "ubuntu_for_haskell",
    digest = "sha256:6a4c2444a7644907e0b523baf9d4516d0fe8c573d0165ce52ea9e38e4d096909",
    registry = "ghcr.io",
    repository = "matsubara0507/ubuntu-for-haskell",
)
