let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies = conf.dependencies # [ "assert", "console", "effect" ]
        , backend =
            ''
            pslua \
            --foreign-path . \
            --ps-output output \
            --lua-output-file dist/test.lua \
            --entry Test.Main
            ''
        }
