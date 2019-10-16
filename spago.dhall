{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "cloud-juice"
, dependencies =
    [ "affjax"
    , "aff-retry"
    , "avar"
    , "console"
    , "debug"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "formatters"
    , "flow-id"
    , "node-fs"
    , "node-http"
    , "node-process"
    , "node-streams"
    , "nullable"
    , "prelude"
    , "psci-support"
    , "quickcheck"
    , "simple-json"
    , "spec-discovery"
    , "spec-quickcheck"
    , "transformers"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
