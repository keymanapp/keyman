#/usr/bin/env bash

_km-package-uninstall_completions()
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="-h -s -v -vv --version"

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    fi

    if [[ "${#COMP_WORDS[@]}" != "2" ]]; then
        if [[ ${prev} != -* ]]; then
            return 0
        fi
    fi

    words=""
    shared=""
    case "${prev}" in
        "-s")
            for file in `find /usr/local/share/keyman/ -name *.kmx`; do kbid="`basename ${file} .kmx`"; shared="${shared} ${kbid}"; done
            COMPREPLY=($(compgen -W "${shared}" -- ${cur}))
            ;;
        *)
        for file in `find ~/.local/share/keyman/ -name *.kmx`; do kbid="`basename ${file} .kmx`"; words="${words} ${kbid}"; done
        COMPREPLY=($(compgen -W "${words}" -- ${cur}))
        ;;
    esac
}

_km-package-install_completions()
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="-h --help -v -vv --version -k -f -s"

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    fi

    case "${prev}" in
        "-k")
            words=""
            if [[ ! -e ~/.cache/keyman/kmpdirlist ]] ; then
                if [[ -e ./km-package-install ]]; then
                    python3 -c "from imp import load_source;load_source('km_package_install', './km-package-install');from km_package_install import list_keyboards;list_keyboards()"
                else
                    python3 -c "from imp import load_source;load_source('km_package_install', '/usr/bin/km-package-install');from km_package_install import list_keyboards;list_keyboards()"
                fi
            fi

            if [[ -r ~/.cache/keyman/kmpdirlist ]] ; then
                for file in `cat ~/.cache/keyman/kmpdirlist`; do words="${words} ${file}"; done
                COMPREPLY=($(compgen -W "${words}" -- ${cur}))
                return 0
            fi
            ;;
        *)
        ;;
    esac
}

_km-package-list-installed_completions()
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="-h --help -l --long -v -vv --version -u --user -o --os -s --shared"

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    fi
}

complete -F _km-package-uninstall_completions km-package-uninstall
complete -F _km-package-install_completions km-package-install
complete -F _km-package-list-installed_completions km-package-list-installed
