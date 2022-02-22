#!/usr/bin/env bash
set -eo pipefail
[[ $(fc-list | grep -i fira) != "" ]] && exit 0

fonts_dir="${HOME}/.local/share/fonts"
if [ ! -d "${fonts_dir}" ]; then
    mkdir -p "${fonts_dir}"
fi

version=5.2
zip=Fira_Code_v${version}.zip
curl --fail --location --show-error https://github.com/tonsky/FiraCode/releases/download/${version}/${zip} --output ${zip}
unzip -o -q -d ${fonts_dir} ${zip}
rm ${zip}

# for now we need the Symbols font, too
zip=FiraCode-Regular-Symbol.zip
curl --fail --location --show-error https://github.com/tonsky/FiraCode/files/412440/${zip} --output ${zip}
unzip -o -q -d ${fonts_dir} ${zip}
rm ${zip}

fc-cache -f
