#!/bin/bash
set -xeo pipefail

GHC="ghc-8.6"
CABAL="cabal-install-2.4"

#apt-get update -q

# apt-get install -yq $CABAL $GHC
cabal --version
ghc --version
