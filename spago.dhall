{ name = "my-project"
, dependencies =
  [ "aff"
  , "chanterelle"
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "identity"
  , "integers"
  , "maybe"
  , "node-process"
  , "prelude"
  , "profunctor-lenses"
  , "tagged"
  , "transformers"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}