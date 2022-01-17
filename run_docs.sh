#!/usr/bin/bash

cd ../plutus-apps/result/share/doc/
pushd index.html;  python3 -m http.server 9999; popd;
