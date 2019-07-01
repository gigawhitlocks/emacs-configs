#!/bin/bash

set -exo pipefail

# jump to a tempdir
pushd "$(mktemp -d)"

# select a mirror
mirror=$(curl https://ftpmirror.gnu.org/gnu/emacs -v 2>&1 \
	     | grep -e "Location: http" | awk '{print $NF}')

mirror=$(echo $mirror)

curl $mirror


# get the latest emacs version from a GNU mirror 
latest=$(curl $mirror 2>/dev/null -o - \
	     | grep -e "emacs.*tar.*gz" \
	     | cut -d\" -f2 \
	     | grep -v intro \
	     | grep -e z$ \
	     | tail -n1)

# download that version of emacs from the mirror
