# prompt-pcap [![Build Status](https://secure.travis-ci.org/jtobin/prompt-pcap.png?branch=master)](http://travis-ci.org/jtobin/prompt-pcap) 

A simple pcap parser.  Requires libpcap-dev, which can be installed by

    sudo apt-get update && sudo apt-get install libpcap-dev

on Debian and Ubuntu.  It should already be present on an OS X installation.  Install prompt-pcap via:

    git clone git@github.com:jtobin/prompt-pcap.git
    cd prompt-pcap
    cabal install
 
Take note of where the executable `prompt-pcap` will be installed, and add this to your PATH if necessary. 

Example usage:

    $ prompt-pcap
    A Kospi Quote Parser
    
    Usage: prompt-pcap [-r|--reorder] PCAPFILE
      Parse a pcap file according to spec.
    
    Available options:
      -h,--help                Show this help text
      -r,--reorder             Reorder quotes by accept time.

    $ prompt-pcap -r mdf-kospi200.20110216-0.pcap | tail -3
    2011-02-16 00:00:29.9757 UTC 2011-02-16 00:00:29.95 UTC KR4301F32570 42@223 49@224 43@225 128@226 118@227 82@228 151@229 176@230 53@231 5@232
    2011-02-16 00:00:29.9960 UTC 2011-02-16 00:00:29.97 UTC KR4201F32721 519@138 246@139 668@140 75@141 5@142 39@143 62@144 78@145 74@146 75@147
    2011-02-16 00:00:29.9985 UTC 2011-02-16 00:00:29.97 UTC KR4301F32505 134@92 199@93 231@94 94@95 308@96 234@97 130@98 282@99 415@100 52@101


