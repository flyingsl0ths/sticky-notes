#!/usr/bin/env bash

data=(
    "{\"title\": \"Hello\", \"text\": \"Upgradable\", \"author\": \"Jaime10\", \"date\": $(date +%s)}"  \
    "{\"title\": \"intermediate\", \"text\": \"interface circuit Parks\", \"author\": \"Bertha.Prohaska74\", \"date\": $(date +%s)}"\
    "{\"title\": \"Cross-Platform\", \"text\": \"Intelligent\", \"author\": \"Louie49\", \"date\": $(date +%s)}"      \
    "{\"title\": \"Hacking\", \"text\": \"Configurable\", \"author\": \"Roy53\", \"date\": $(date +%s)}" \
    "{\"title\": \"Maximized\", \"text\": \"Lead real-time\", \"author\": \"Marley_Rath\", \"date\": $(date +%s)}"
    "{\"title\": \"Systemic\", \"text\": \"value-added 4th\", \"author\": \"Cloyd.Orn41\", \"date\": $(date +%s)}"
    "{\"title\": \"Hybrid\", \"text\": \"navigating core\", \"author\": \"Leonor.Murphy\", \"date\": $(date +%s)}"  \
    "{\"title\": \"Fresh\", \"text\": \"fault-tolerant moderator Kentucky\", \"author\": \"Kamren_Wintheiser\", \"date\": $(date +%s)}"\
    "{\"title\": \"web-readiness\", \"text\": \"withdrawal bandwidth copy\", \"author\": \"Krystina.McKenzie\", \"date\": $(date +%s)}"      \
    "{\"title\": \"Refined\", \"text\": \"expedite Berkshire Fresh\", \"author\": \"Miracle41\", \"date\": $(date +%s)}" \
    "{\"title\": \"Corporate\", \"text\": \"Steel\", \"author\": \"Caitlyn45\", \"date\": $(date +%s)}" \
    "{\"title\": \"Infrastructure\", \"text\": \"override Mayotte Wooden\", \"author\": \"Benedict.Bergstrom48\", \"date\": $(date +%s)}" \
    )
    
domain=5000

for json in "${data[@]}"; do
    result=$(curl --silent -X POST -H "Content-Type: application/json" -d "$json" "localhost:$domain/note")
    printf "%s\n" "$result"
done
