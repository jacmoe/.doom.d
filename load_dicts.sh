#!/usr/bin/env bash

DIC_FOLDER=~/stardict/dic
if [ ! -d "$DIC_FOLDER" ]; then
    TMP="$(mktemp -d /tmp/dict-XXX)"
    cd "$TMP"
    curl -A "Mozilla/4.0" -o "stardict.tar.gz" "https://tecosaur.com/resources/config/stardict.tar.gz"
    tar -xf "stardict.tar.gz"
    rm "stardict.tar.gz"
    mkdir -p "$DIC_FOLDER"
    mv * "$DIC_FOLDER"
fi
