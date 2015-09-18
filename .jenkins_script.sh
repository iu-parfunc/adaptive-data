#!/bin/bash


which -a stack
stack --version
stack --install-ghc --no-system-ghc build
