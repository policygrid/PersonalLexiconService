#!/bin/sh

gawk '/^<w>/ {gsub(/w> .*$/, "w> NP")} {print}'
