#!/bin/bash

set -eo pipefail

if [[ -e /tmp/.emacs-build-cache ]]; then
    # jump to cache
    echo "Using cache.."
    cd $(cat /tmp/.emacs-build-cache)
else
    # jump to a tempdir
    echo "New install.."
    pushd "$(mktemp -d)"
    echo $(pwd) > /tmp/.emacs-build-cache
fi

# select a mirror
mirror=$(curl https://ftpmirror.gnu.org/gnu/emacs -v 2>&1 \
	     | grep -e "Location: http" | awk '{print $NF}' \
	     | tr -d '\r' | tr -d '$')

# get the latest emacs version from a GNU mirror 
function get_latest () {
    curl -L $mirror 2>/dev/null -o - \
	| grep -e "emacs.*tar.*gz" \
	| cut -d\" -f2 \
	| grep -v intro \
	| grep -e "gz$" \
	| tail -n1
}

latest=$(get_latest)
# retry once max
[[ $latest == "" ]] && latest=$(get_latest)

latest_no_suffix=$(echo $latest | sed 's/\.tar\.gz//g')

# download that version of emacs from the mirror
[[ ! -e $latest_no_suffix ]] && curl "${mirror}/${latest}" -o - | tar xz
pushd $latest_no_suffix 

# install dependencies
if [[ -e /etc/lsb-release ]]; then
    echo "Probably Ubuntu.. attempting install with apt-get.."
    echo "Enabling ALL SOURCE REPOS"
    sudo sed -i 's/^# deb-src/deb-src/g' /etc/apt/sources.list
    sudo apt-get update -yq
    sudo apt-get build-dep -yq emacs
else
    echo "No automatic dependency install for this platform yet."
    exit 1
fi

# build emacs
make -j4 || ./configure
make -j4
sudo make install
