import { fileURLToPath } from "url";
import { dirname } from 'path';

// Tells ts-node where to find the tsconfig.json to be used for executing TS unit tests.
process.env.TS_NODE_PROJECT = `${dirname(fileURLToPath(import.meta.url))}/src/test/auto/tsconfig.json`;