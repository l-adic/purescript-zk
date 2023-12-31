{ name = "my-project"
, dependencies =
  [ "either"
  , "identity"
  , "lists"
  , "maybe"
  , "prelude"
  , "tagged"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
