#!/bin/bash

set -e
set -u

THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
THIS_DIR=$(dirname "$THIS_SCRIPT")

cd "${THIS_DIR}"

function display_usage {
    echo "Usage: $0 [--man] [--md] [--no-reconf]"
    echo "       $0 --help"
    echo
    echo "  --help       displays this screen and exits"
    echo "  --man        generate man pages from python files"
    echo "  --md         generate markdown help pages from python files"
    echo "  --no-reconf  don't run reconf.sh first"
    echo
    echo "If neither --man nor --md is specified, both man and help pages are generated."
}

generate_man=
generate_help=
reconfigure=1

while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        --help|-h)
            display_usage
            exit 0
            ;;
        --man)
            generate_man=1
            ;;
        --md)
            generate_help=1
            ;;
        --no-reconf)
            reconfigure=
            ;;
        *)
            echo "$0: invalid option: $key"
            display_usage
            exit 1
    esac
    shift # past the processed argument
done

if [ -z "$generate_man" ] && [ -z "$generate_help" ]; then
    generate_man=1
    generate_help=1
fi

if [ -n "$reconfigure" ]; then
    pushd "$THIS_DIR/.." > /dev/null
    ./scripts/reconf.sh
    popd > /dev/null
fi

if [ -n "$generate_man" ]; then
    echo "Generating man pages..."
    mkdir -p ../../debian/man
    buildtools/help2man -N ./km-package-get -o ../../debian/man/km-package-get.1 -n "Download a Keyman keyboard package" -I maninc/km-package-get.inc
    buildtools/help2man -N ./km-package-install -o ../../debian/man/km-package-install.1 -n "Install a Keyman keyboard package" -I maninc/km-package-install.inc
    buildtools/help2man -N ./km-config -o ../../debian/man/km-config.1 -n "Launches Keyman Configuration for installing and showing information about Keyman keyboards" -I maninc/km-config.inc
    buildtools/help2man -N ./km-kvk2ldml -o ../../debian/man/km-kvk2ldml.1 -n "Convert a Keyman on-screen keyboard file to LDML" -I maninc/km-kvk2ldml.inc
    buildtools/help2man -N ./km-package-list-installed -o ../../debian/man/km-package-list-installed.1 -n "List installed Keyman keyboard packages" -I maninc/km-package-list-installed.inc
    buildtools/help2man -N ./km-package-uninstall -o ../../debian/man/km-package-uninstall.1 -n "Uninstall a Keyman keyboard package" -I maninc/km-package-uninstall.inc
fi

if [ -n "$generate_help" ]; then
    echo "Generating markdown help pages..."
    mkdir -p ../docs/help/reference
    buildtools/help2md ./km-package-get -o ../docs/help/reference/km-package-get.md -n "Download a Keyman keyboard package" -I maninc/km-package-get.inc
    buildtools/help2md ./km-package-install -o ../docs/help/reference/km-package-install.md -n "Install a Keyman keyboard package" -I maninc/km-package-install.inc
    buildtools/help2md ./km-config -o ../docs/help/reference/km-config.md -n "Launches Keyman Configuration for installing and showing information about Keyman keyboards" -I maninc/km-config.inc
    buildtools/help2md ./km-kvk2ldml -o ../docs/help/reference/km-kvk2ldml.md -n "Convert a Keyman on-screen keyboard file to LDML" -I maninc/km-kvk2ldml.inc
    buildtools/help2md ./km-package-list-installed -o ../docs/help/reference/km-package-list-installed.md -n "List installed Keyman keyboard packages" -I maninc/km-package-list-installed.inc
    buildtools/help2md ./km-package-uninstall -o ../docs/help/reference/km-package-uninstall.md -n "Uninstall a Keyman keyboard package" -I maninc/km-package-uninstall.inc
fi
