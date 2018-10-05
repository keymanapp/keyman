#/usr/bin/env bash

_km-package-uninstall_completions()
{
#  if [ "${#COMP_WORDS[@]}" != "2" ]; then
#    return
#  fi

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

complete -F _km-package-uninstall_completions km-package-uninstall
