let upstream-ps =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260605/packages.dhall
        sha256:e48c9b283ca89ec994453459fb74c4b5b5a9432349f83a2e104f39dd869a0f6e

let upstream-lua =
      https://github.com/purescript-lua/purescript-lua-package-sets/releases/download/psc-0.15.15-20260613/packages.dhall
        sha256:4cb4784187583587818384ca3c4930f8fe77b15796ff7d487f628ef4590d8058

in  upstream-ps // upstream-lua
