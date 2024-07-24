#!/bin/bash

# If needed set cowbuilder up for building Keyman Debian packages
# Then cowbuilder update

distributions='focal jammy noble'

if ! dpkg-query -l cowbuilder; then
    echo "installing pbuilder and cowbuilder"
    sudo apt install pbuilder cowbuilder
else
    echo "already have pbuilder and cowbuilder"
fi

# set -e at this point to allow detecting the error of the dpkg-query
set -e

if [ ! -e ~/.pbuilderrc ]; then
    echo "linking .pbuilderrc to your ~/.pbuilderrc"
    ln -s "$(pwd)/.pbuilderrc" ~/.pbuilderrc
else
    echo "assuming you already have ~/.pbuilderrc set up as you want it"
    echo "though you may like to look at this .pbuilderrc in case it is useful"
fi

for dist in $distributions; do
    sudo mkdir -p /var/cache/pbuilder/result/"${dist}"
    if [ ! -e /var/cache/pbuilder/result/"${dist}"/Packages ]; then
        sudo touch /var/cache/pbuilder/result/"${dist}"/Packages
    fi
    sudo mkdir -p /var/cache/pbuilder/hook.d/"${dist}"
    if [ ! -e /var/cache/pbuilder/hook.d/"${dist}"/D70results ]; then
        echo "#!/bin/bash" | sudo tee /var/cache/pbuilder/hook.d/"${dist}"/D70results
        echo "cd /var/cache/pbuilder/result/${dist}" | sudo tee -a /var/cache/pbuilder/hook.d/"${dist}"/D70results
        echo "/usr/bin/dpkg-scanpackages . /dev/null > /var/cache/pbuilder/result/${dist}/Packages" | sudo tee -a /var/cache/pbuilder/hook.d/"${dist}"/D70results
        echo "/usr/bin/apt-get update" | sudo tee -a /var/cache/pbuilder/hook.d/"${dist}"/D70results
        sudo chmod +x /var/cache/pbuilder/hook.d/"${dist}"/D70results
        sudo ln -s /var/cache/pbuilder/hook.d/"${dist}"/D70results /var/cache/pbuilder/hook.d/"${dist}"/I70buildresults
    fi
    if [ ! -e /var/cache/pbuilder/hook.d/"${dist}"/D80no-man-db-rebuild ]; then
        sudo ln -s /usr/share/doc/pbuilder/examples/D80no-man-db-rebuild /var/cache/pbuilder/hook.d/"${dist}"/D80no-man-db-rebuild
    fi
    if [ ! -d /var/cache/pbuilder/base-"${dist}".cow ]; then
        echo "making ${dist} cowbuilder"
        sudo DIST="${dist}" cowbuilder --create --distribution "${dist}" --basepath /var/cache/pbuilder/base-"${dist}".cow  --hookdir /var/cache/pbuilder/hook.d/"${dist}"
    else
        echo "already have ${dist} cowbuilder"
    fi
    sudo DIST="${dist}" cowbuilder --update --distribution "${dist}" --basepath /var/cache/pbuilder/base-"${dist}".cow --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/"${dist}" --hookdir /var/cache/pbuilder/hook.d/"${dist}"
done
