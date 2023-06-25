let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230604/packages.dhall
        sha256:e4f864d9214f5b8c2f6de4964794f0568966b83a51173dd76dbe9e1f3be4965f

in  upstream
  with katex = 
    { repo = "https://github.com/kentookura/purescript-deku-katex.git"
    , version = "ca1d44704c9a8eb25621a8313be157065b92949b"
    , dependencies = 
      [ "console"
      , "deku"
      , "effect"
      , "foreign"
      , "hyrule"
      , "ordered-collections"
      , "prelude"
      , "tuples"
      , "web-dom"
      ]
    }
