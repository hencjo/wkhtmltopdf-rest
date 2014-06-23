#!/bin/bash
cabal build && strip dist/build/pdf/pdf && upx -qq dist/build/pdf/pdf

