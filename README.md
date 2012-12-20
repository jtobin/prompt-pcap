# prompt-pcap [![Build Status](https://secure.travis-ci.org/jtobin/prompt-pcap.png?branch=master)](http://travis-ci.org/jtobin/prompt-pcap) 

A simple pcap parser.  Requires libpcap-dev, which can be installed by

    sudo apt-get update && sudo apt-get install libpcap-dev

on Debian and Ubuntu.  It should be present on an OS X installation.

Install prompt-pcap via:

    git clone git@github.com:jtobin/prompt-pcap.git
    cd prompt-pcap
    cabal configure
    cabal build
 
The executable will be built in `dist/build/prompt-pcap`.  Run it to see usage instructions.

