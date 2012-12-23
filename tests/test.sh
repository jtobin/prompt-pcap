#!/bin/bash
# unit tests for prompt-pcap

# do both options return the same number of quotes?
vanillaLines=$(./dist/build/prompt-pcap/prompt-pcap ./data/mdf-kospi200.20110216-0.pcap | wc -l)
reorderLines=$(./dist/build/prompt-pcap/prompt-pcap ./data/mdf-kospi200.20110216-0.pcap -r | wc -l)

# does -r reorder correctly?
$(./dist/build/prompt-pcap/prompt-pcap ./data/mdf-kospi200.20110216-0.pcap -r | cut -f 4-5 -d ' ' | sort -c)
ordered=$?

# test summary
[[ ($vanillaLines = $reorderLines) && $ordered ]] && echo "All tests passed."

