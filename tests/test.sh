#!/bin/bash
# does -r reorder correctly?
./dist/build/prompt-pcap/prompt-pcap -r ./data/mdf-kospi200.20110216-0.pcap | cut -f 4-5 -d ' ' | sort -c
