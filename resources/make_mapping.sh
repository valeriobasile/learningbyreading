#!/bin/sh

:> wn16-30
cat mappings-upc-2007/mapping-16-30/wn16-30.noun | cut -d' ' -f 1,2 | sed -e "s/ /-n /" -e "s/$/-n/" >> wn16-30
cat mappings-upc-2007/mapping-16-30/wn16-30.verb | cut -d' ' -f 1,2 | sed -e "s/ /-v /" -e "s/$/-v/" >> wn16-30
cat mappings-upc-2007/mapping-16-30/wn16-30.adj  | cut -d' ' -f 1,2 | sed -e "s/ /-a /" -e "s/$/-a/" >> wn16-30
cat mappings-upc-2007/mapping-16-30/wn16-30.adv  | cut -d' ' -f 1,2 | sed -e "s/ /-r /" -e "s/$/-r/" >> wn16-30
