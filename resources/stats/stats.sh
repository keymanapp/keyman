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

builder_describe "Stats collector for Keyman PRs and issues

* You must specify either --start-date (-d) or --start-sprint (-s).
" \
  stats+ \
  "--start-date,-d=startDate       Start date for collecting data, yyyy-mm-dd" \
  "--start-sprint,-s=startSprint   Or, first sprint to start collecting data for" \
  "--stable,-S=stableBranch        Branch name for previous stable release (required)"

builder_parse "$@"

if builder_has_option --start-sprint; then
  SPRINT_END_DATE=$(gh api -X GET \
    repos/keymanapp/keyman/milestones -f direction=desc -f sort=due_on -f state=all --paginate \
    --jq '.[] | select(.title == "'${startSprint}'").due_on'
  )
  if [[ -z "$SPRINT_END_DATE" ]]; then
    builder_die "Sprint $startSprint not found on GitHub, or it has no due date"
  fi
  START_DATE=$(date --date "$SPRINT_END_DATE -11 days" '+%Y-%m-%d')
elif builder_has_option --start-date; then
  START_DATE="$startDate"
else
  builder_die "Either --start-date or --start-sprint must be specified"
fi

if ! builder_has_option --stable; then
  builder_die "--stable must be specified"
fi

echo "Collecting statistics for sprints starting on $START_DATE, and with stable branch $stableBranch"

STATS_ROOT="$KEYMAN_ROOT/resources/stats/data/"
mkdir -p "$STATS_ROOT"

echo -e "Platform\tAll PRs\t${stableBranch}\tFeatures\tIssues" > "${STATS_ROOT}stats.tsv"

for p in "${platforms[@]}"; do
  echo "Getting stats for $p"
  mkdir -p "$STATS_ROOT/$p"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$START_DATE -label:auto label:$p" --state all > "$STATS_ROOT/${p}pulls.tsv"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "base:$stableBranch created:>$START_DATE -label:auto label:$p" --state all > "$STATS_ROOT/${p}pulls-stable.tsv"
  gh pr list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$START_DATE -label:auto label:feat label:$p" --state all > "$STATS_ROOT/${p}pulls-feat.tsv"
  gh issue list --json author,createdAt,number,title,labels,state --template "$(cat gh-pr.txt)" --limit 1000 --search "created:>$START_DATE -label:auto label:$p" --state all > "$STATS_ROOT/${p}issues.tsv"
  stat_all=`cat "$STATS_ROOT/${p}pulls.tsv" | wc -l`
  stat_stable=`cat "$STATS_ROOT/${p}pulls-stable.tsv" | wc -l`
  stat_feat=`cat "$STATS_ROOT/${p}pulls-feat.tsv" | wc -l`
  stat_issues=`cat "$STATS_ROOT/${p}issues.tsv" | wc -l`
  echo -e "$p\t$stat_all\t$stat_stable\t$stat_feat\t$stat_issues" >> "${STATS_ROOT}stats.tsv"
done
