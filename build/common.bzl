DEVELOPMENT_GHC_FLAGS = ["-O0"]
RELEASE_GHC_FLAGS = ["-O1"]

GHC_FLAGS = [
    "-v1",
    "-j8",
    "-fdiagnostics-color=always",
    "-ferror-spans",
    "-Weverything",
    "-Wno-missing-local-signatures",
    "-Wno-missing-import-lists",
    "-Wno-implicit-prelude",
    "-Wno-safe",
    "-Wno-unsafe",
    "-Wno-name-shadowing",
    "-Wno-monomorphism-restriction",
    "-Wno-missed-specialisations",
    "-Wno-all-missed-specialisations",
    "-Wno-star-is-type",
    "-Wno-missing-deriving-strategies",
    "-DBAZEL_BUILD=1",
    "-Wno-unused-packages",
    "-Wno-prepositive-qualified-module",
    "-Wno-missing-safe-haskell-mode",
    "-XNoImplicitPrelude",
    "-XConstraintKinds",
    "-XDataKinds",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XLambdaCase",
    "-XMultiWayIf",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPolyKinds",
    "-XRankNTypes",
    "-XStandaloneDeriving",
    "-XTypeFamilies",
    "-XTypeOperators",
    "-XTypeSynonymInstances",
] + select(
    {
        "//:release": RELEASE_GHC_FLAGS,
        "//:development": DEVELOPMENT_GHC_FLAGS,
        "//:debug": DEVELOPMENT_GHC_FLAGS,
    },
)
