#!/usr/bin/bash

cd /vagrant/cardano/plutus-apps/result/share/doc/
pushd index.html;  python3 -m http.server 9999; popd;
