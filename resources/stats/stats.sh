#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

cd "$THIS_SCRIPT_PATH"

platforms=(android/ ios/ linux/ mac/ web/ windows/ developer/ core/ common/models/)

startdate=2022-05-01
stable=stable-15.0
STATS_ROOT="$KEYMAN_ROOT/resources/stats/data/"
mkdir -p "$STATS_ROOT"

echo -e "Platform\tAll PRs\t${stable}\tFeatures\tIssues" > "${STATS_ROOT}stats.tsv"

for p in "${platforms[@]}"; do
  echo "Getting stats for $p"
  mkdir -p "$STATS_ROOT/$p"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$startdate -label:auto label:$p" --state all > "$STATS_ROOT/${p}pulls.tsv"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "base:$stable created:>$startdate -label:auto label:$p" --state all > "$STATS_ROOT/${p}pulls-stable.tsv"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$startdate -label:auto label:feat label:$p" --state all > "$STATS_ROOT/${p}pulls-feat.tsv"
  gh issue list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$startdate -label:auto label:$p" --state all > "$STATS_ROOT/${p}issues.tsv"
  stat_all=`cat "$STATS_ROOT/${p}pulls.tsv" | wc -l`
  stat_stable=`cat "$STATS_ROOT/${p}pulls-stable.tsv" | wc -l`
  stat_feat=`cat "$STATS_ROOT/${p}pulls-feat.tsv" | wc -l`
  stat_issues=`cat "$STATS_ROOT/${p}issues.tsv" | wc -l`
  echo -e "$p\t$stat_all\t$stat_stable\t$stat_feat\t$stat_issues" >> "${STATS_ROOT}stats.tsv"
done
