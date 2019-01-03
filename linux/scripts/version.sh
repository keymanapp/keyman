#!/bin/bash

version()
{
    if [[ -z "${TIER}" ]]; then
        tier="alpha"
    else
        tier="${TIER}"
    fi
    echo "tier is ${tier}"
    local catvers=`cat VERSION`
    echo "VERSION is ${catvers}"
    local datevers=`TZ=UTC git log -1 --pretty=format:%cd --date=format-local:%Y%m%d%H%M`
    local num=`git describe --abbrev=0 --match=linux-release-${tier}*| cut -d '.' -f3`
    if [[ -z "${num}" ]]; then
        num="0"
    fi
    echo "current tag number ${num}"
    local next=$((num+1))
    echo "next tag number ${next}"

    local current_tag=`git tag -l --points-at HEAD|grep "linux-release-${tier}"|grep "${catvers}.${num}"`

    if [ -n "${current_tag}" ]; then
        echo "tag is on current commit, use $num"
        newvers="${catvers}.${num}"
    else
        #echo "tag is not on current commit"
        local prev_tag=`git tag --contains HEAD~1|grep "linux"|grep ${num}`
        if [ -n "${prev_tag}" ]; then
            #echo "tag is on previous commit"
            local prev_log=`git log --oneline HEAD~1..HEAD|grep "linux"|grep "Merge pull"`
            if [ -n "${prev_log}" ]; then
                echo "latest commit was a merge PR, use $num"
                newvers="${catvers}.${num}"
            else
                if [ "${JENKINS}" == "no" ]; then
                    echo "latest commit was not a merge PR, use $num and datetime"
                    newvers="${catvers}.${num}.${datevers}"
                else
                    echo "latest commit was not a merge PR, on jenkins use $next because not tagged yet"
                    newvers="${catvers}.${next}"
                fi
            fi
        else
            if [ "${JENKINS}" == "no" ]; then
                echo "tag is not on previous commit, use $num and datetime"
                newvers="${catvers}.${num}.${datevers}"
            else
                echo "tag is not on previous commit, on jenkins use $next because not tagged yet"
                newvers="${catvers}.${next}"
            fi
        fi
    fi
}
