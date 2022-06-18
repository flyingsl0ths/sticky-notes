#!/bin/bash

DB_FILE="debug.db"

ST_DB="$DB_FILE" cabal v2-run sticky-notes:sticky-notes

rm "debug.db"
