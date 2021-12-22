const pjson = require('../package.json');

function extractVersionData(version: string) {
  const versionData = /(\d+\.\d+\.\d+)(?:-(alpha|beta))?(?:-(test|local))?(?:-(\d+))?/.exec(version);
  if(!versionData) return null;
  return {
    version: versionData[1],
    tier: versionData[2] || 'stable',
    environment: versionData[3] || versionData[2] || 'stable',
    pr: versionData[4] || ''
  };
};

let versionData = extractVersionData(pjson.version);

export interface Environment {
  environment: string;
  version: string;
  tier: string;
  versionWithTag: string;
}

export const environment: Environment = {
  environment: versionData.environment,
  version: versionData.version,
  tier: versionData.tier,
  versionWithTag: pjson.version
}