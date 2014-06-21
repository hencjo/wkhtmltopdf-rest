#!/bin/bash
cabal build && dist/build/pdf/pdf backend.conf
