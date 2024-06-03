import { testBuildConfigurations } from './trigger_definitions.mjs';

if(process.argv.length != 4) {
  console.error('Invalid parameters!');
  console.error('Usage: node cancel-test-builds.mjs PR|branch TEAMCITY_TOKEN');
  console.error('  PR number should be a pull request number, without preceding #');
  console.error('  branch can be one of master, beta or stable-x.y, where x.y is a stable release');
  process.exit(1);
}

const allTestBuildConfigurations = Object.keys(testBuildConfigurations).flatMap(item => testBuildConfigurations[item]);
const branchName = process.argv[2];
const authorizationHeader = "Bearer "+process.argv[3];

console.log(`Cancelling all queued and running test builds for ${branchName}`);

async function getTeamCity(api) {
  const response = await fetch('https://build.palaso.org/app/rest/'+api, {
    headers: {
      "Authorization": authorizationHeader,
      "Accept": "application/json"
    }
  });
  if(!response.ok) {
    return null;
  }
  return await response.json();
}

async function postTeamCity(api, body) {
  const response = await fetch('https://build.palaso.org/app/rest/'+api, {
    method: 'POST',
    headers: {
      "Authorization": authorizationHeader,
      "Content-Type": "application/json",
    },
    body: body
  });
  if(!response.ok) {
    console.warn(`API ${api} failed with HTTP/${response.status}: ${response.statusText}`);
    return null;
  }
  return true;
}

async function getQueuedBuilds() {
  return await getTeamCity('buildQueue?locator=count:200');
}

async function cancelQueuedBuild(id, buildTypeId) {
  console.log(`Cancelling queued build ${id} on ${branchName} for ${buildTypeId}`);
  if(await postTeamCity('buildQueue/id:'+id, `{ "buildCancelRequest": { "comment": "Cancelling build due to new commits on branch.", "readdIntoQueue": "false" } }`)) {
    console.log('  Build successfully cancelled.');
  }
}

async function getRunningBuilds() {
  return await getTeamCity('builds?locator=running:true,count:200');
}

async function cancelRunningBuild(id, buildTypeId) {
  console.log(`Cancelling running build ${id} on ${branchName} for ${buildTypeId}`);
  if(await postTeamCity('builds/id:'+id, `{ "buildCancelRequest": { "comment": "Cancelling build due to new commits on branch.", "readdIntoQueue": "false" } }`)) {
    console.log('  Build successfully cancelled.');
  }
}

try {
  const queuedBuilds = await getQueuedBuilds();
  if(queuedBuilds) {
    for(const build of queuedBuilds.build) {
      if(allTestBuildConfigurations.includes(build.buildTypeId) && build.branchName == branchName) {
        await cancelQueuedBuild(build.id, build.buildTypeId);
      }
    }
  }

  const runningBuilds = await getRunningBuilds();
  if(runningBuilds) {
    for(const build of runningBuilds.build) {
      if(allTestBuildConfigurations.includes(build.buildTypeId) && build.branchName == branchName) {
        await cancelRunningBuild(build.id, build.buildTypeId);
      }
    }
  }
} catch(e) {
  // we don't want to fail a trigger because of a hiccup in this script
  console.trace(e);
  process.exit(0);
}