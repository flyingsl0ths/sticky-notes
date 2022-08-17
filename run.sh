#!/usr/bin/env bash

## THE VALUES HERE ARE MEANT FOR TESTING PURPOSES ONLY
## DO NOT USE IN PRODUCTION

DB_FILE="debug.db"
ADMIN_HASH=$(cabal v2-run sticky-notes:hash | tail -n 1)

ST_DB="$DB_FILE" ST_ADMIN_PASSWORD_HASH="$ADMIN_HASH" cabal v2-run sticky-notes:sticky-notes
