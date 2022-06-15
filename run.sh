#!/bin/bash

LOG_FILE="debug.log"
DB_FILE="debug.db"

ST_DB="$DB_FILE" ST_SVR_LOG="$LOG_FILE" cabal v2-run sticky-notes:sticky-notes

for f in debug.*; do
  rm "$f"
done
