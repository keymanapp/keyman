#!/bin/bash

# If needed set cowbuilder up for building Keyman Debian packages
# Then cowbuilder update

set -e

distributions='bionic xenial'


dpkgcheck=`dpkg-query -l cowbuilder`
if [ $? != 0 ]; then

    echo "installing pbuilder and cowbuilder"
    sudo apt install pbuilder cowbuilder
else
    echo "already have pbuilder and cowbuilder"
fi

for dist in $distributions; do
    if [ ! -d /var/cache/pbuilder/base-${dist}.cow ]; then
        echo "making ${dist} cowbuilder"
        sudo cowbuilder create --distribution ${dist} --basepath /var/cache/pbuilder/base-${dist}.cow  --hookdir /var/cache/pbuilder/hook.d/${dist}
    else
        echo "already have ${dist} cowbuilder"
    fi
    sudo mkdir -p /var/cache/pbuilder/hook.d/${dist}
    if [ ! -e /var/cache/pbuilder/hook.d/${dist}/D70results ]; then
        sudo echo "#!/bin/bash" > /var/cache/pbuilder/hook.d/${dist}/D70results
        sudo echo "cd /var/cache/pbuilder/result/bionic" >> /var/cache/pbuilder/hook.d/${dist}/D70results
        sudo echo "/usr/bin/dpkg-scanpackages . /dev/null > /var/cache/pbuilder/result/bionic/Packages" >> /var/cache/pbuilder/hook.d/${dist}/D70results
        sudo echo "/usr/bin/apt-get update" >> /var/cache/pbuilder/hook.d/${dist}/D70results
        sudo ln -s /var/cache/pbuilder/hook.d/${dist}/D70results /var/cache/pbuilder/hook.d/${dist}/B70buildresults
    fi
    sudo mkdir -p /var/cache/pbuilder/result/${dist}
    sudo cowbuilder update --distribution ${dist} --basepath /var/cache/pbuilder/base-${dist}.cow --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/${dist} --hookdir /var/cache/pbuilder/hook.d/${dist}
done