#!/bin/bash
# Get the tier from TIER.md
# When building from a Linux source package, `TIER.md` is in the
# same directory as this script.
TIERFILE=TIER.md
[ -f ../../../TIER.md ] && TIERFILE=../../../TIER.md
cat $TIERFILE
