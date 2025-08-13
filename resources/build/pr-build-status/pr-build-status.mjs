/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Source script of pr-build-status.yml file for unit testing and development.
 * The indented section of this file below is copied verbatim into
 * .github/workflows/pr-build-status.yml by `build.sh build`, and allows us to
 * develop the YAML script without introducing a dependency on the repository.
 */

// CLONE:START
            // This code is copied out of resources/build/pr-build-status/pr-build-status.mjs
            // where it is tested. It is copied inline here in order to avoid requiring the
            // repository to be checked out, which dramatically reduces the run time of the
            // check.
            //
            // Note: we don't currently look at check runs, only statuses
            //
            // Verify the following statuses:
            //   'user_testing'
            //   'API Verification' (github-actions[bot])
            //
            // At least 1 of the following statuses must be found:
            //   'Test*' (keyman-server), e.g. 'Test Build (Keyman)'
            //   'Ubuntu Packaging' (github-actions[bot])
            //
            // Ignore the following statuses:
            //   check/web/file-size
            //

            function reduceStatuses(statuses) {
              const filtered_statuses = statuses.reduce((o, status) => {
                if(status.creator?.login == 'keyman-server' && status.context.startsWith('Test')) {
                  if(!o[status.context]) o[status.context] = {type: 'build', state: status.state};
                } else if(status.creator?.login == 'keymanapp-test-bot[bot]' && status.context == 'user_testing') {
                  if(!o[status.context]) o[status.context] = {type: 'user-test', state: status.state};;
                } else if(status.context == 'API Verification') {
                  if(!o[status.context]) o[status.context] = {type: 'check', state: status.state};
                } else if(status.context == 'Ubuntu Packaging') {
                  if(!o[status.context]) o[status.context] = {type: 'build', state: status.state};
                } else if(status.context == 'check/web/file-size') {
                  // Ignore check/web/file-size -- we won't block automerge for this at this point
                } else {
                  // We fail with an 'unknown status' response if we get a new status check
                  // so we can be sure we are not skipping known status checks
                  o[status.context] = {type: 'unknown', state: status.state};
                }
                return o;

              }, {});
              return filtered_statuses;
            }

            //
            // Given the collection of status checks we care about, return
            // an aggregate status -- error, failed, pending, or success,
            // and a summary description
            //
            function calculateFinalStatus(filtered_statuses) {
              const counts = {};
              let hasBuilds = false;
              for(const context of Object.keys(filtered_statuses)) {
                const { state, type } = filtered_statuses[context];
                if(type == 'unknown') {
                  // We special-case for unknown status checks, and never permit them
                  return [
                    'error', `An unknown context ${context} was found, cannot calculate build status.`
                  ];
                }
                if(type == 'build') {
                  hasBuilds = true;
                }
                counts[state] = counts[state] ? counts[state] + 1 : 1;
              }

              // If we do not have any statuses yet, we wait
              if(Object.keys(filtered_statuses).length == 0 || !hasBuilds) {
                return ['pending', 'Builds have not yet been triggered ⌛'];
              }

              const state =
                counts.error ? 'error' :
                counts.failed ? 'failed' :
                counts.pending ? 'pending' :
                'success';

              let description = '';
              function appendDescription(count, state) {
                if(!count) return;
                if(description != '') description += '; ';
                description += `${count} check${count == 1 ? '' : 's'} ${state}`;
              }
              appendDescription(counts.error, 'in an error state ❌');
              appendDescription(counts.failed, 'failed ❌');
              appendDescription(counts.pending, 'pending ⌛');
              appendDescription(counts.success, 'completed successfully ✅');

              return [ state, description ];
            }

            async function getCommitStatuses(github, owner, repo, sha) {
              const statuses = await github.paginate('GET /repos/{owner}/{repo}/commits/{sha}/statuses', {
                owner,
                repo,
                sha,
                headers: {
                  'X-GitHub-Api-Version': '2022-11-28'
                }
              });
              return statuses;
            }

            async function getCommitCheckRuns(github, owner, repo, sha) {
              const statuses = await github.paginate('GET /repos/{owner}/{repo}/commits/{sha}/check-runs', {
                owner,
                repo,
                sha,
                headers: {
                  'X-GitHub-Api-Version': '2022-11-28'
                }
              });
              return statuses;
            }

            function calculateCheckResult(statuses) {
              if(!Array.isArray(statuses)) {
                return ['error', 'Failed to retrieve status checks from GitHub ❌'];
              }

              const filtered_statuses = reduceStatuses(statuses);

              const result = calculateFinalStatus(filtered_statuses);
              return result;
            }

            async function test(github, owner, repo, sha) {
              // Get statuses from sha
              const statuses = await getCommitStatuses(github, owner, repo, sha);
              return calculateCheckResult(statuses);
            }

            async function createCheck(github, owner, repo, sha) {
              const check = await github.rest.checks.create({
                owner,
                repo,
                head_sha: sha,
                name: 'Build Outcome',
                status: 'in_progress',
              });
              return check.data.id;
            }

            async function updateCheck(github, owner, repo, checkRunId, status, description) {
              const checkStatus = status == 'pending' ? 'in_progress' : 'completed';
              const conclusion = checkStatus == 'in_progress' ? undefined : (status == 'success' ? 'success' : 'failure');

              await github.rest.checks.update({
                owner,
                repo,
                check_run_id: checkRunId,
                status: checkStatus,
                conclusion,
                output: {
                  title: description,
                  summary: ''
                }
              });
            }

// CLONE:END
// CLONE-COMMENTED:START

            // const { owner, repo } = context.repo;
            // const sha =
            //   context.payload?.check_suite?.sha ||    /* check run completed */
            //   context.payload?.inputs?.commit ||      /* manual run */
            //   context.payload?.after ||               /* push */
            //   context.payload?.commit?.sha ||         /* status */
            //   context.sha;                            /* probably 'master'! */

            // const checkRunId = await createCheck(github, owner, repo, sha);
            // const res = await test(github, owner, repo, sha);
            // await updateCheck(github, owner, repo, checkRunId, res[0], res[1]);

// CLONE-COMMENTED:END

// Following code is used only for unit testing; do not include in the .yml

export const unitTestEndpoints = {
  getCommitStatuses, calculateCheckResult, getCommitCheckRuns
};