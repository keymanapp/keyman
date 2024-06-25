import standardConfig from './web-test-runner.config.mjs';
import { buildLegacyTestingConfig } from '@keymanapp/common-test-resources/wtr-browserstack-config.mjs';

const config = buildLegacyTestingConfig(standardConfig);
export default config;