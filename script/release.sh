#!/usr/bin/env bash

set -euxo pipefail

FILE=heka_crawl.csv
TIMESTAMP=$(date -uI)
OUT=${FILE%.csv}_$TIMESTAMP.csv

dune runtest
dune exec hekadump.exe

mv $FILE "$OUT"

# set output parameter for Github workflow
# https://help.github.com/en/actions/automating-your-workflow-with-github-actions/development-tools-for-github-actions#set-an-output-parameter-set-output
echo "::set-output name=output_file::$OUT"
echo "::set-output name=output_path::$(pwd)/$OUT"
