// @ts-check

import js from "@eslint/js";
import typescriptEslint from "typescript-eslint";
import unitTestEndpoints from "./eslint.unitTestEndpoints.mjs";
import { defineConfig } from "eslint/config";

export default defineConfig([{
  extends: [ js.configs.recommended, typescriptEslint.configs.recommended ],
  files: ["src/**/*.ts"],
  plugins: { unitTestEndpoints },
  rules: {
	  "unitTestEndpoints/prohibit-unitTestEndpoints": "error",
	},
},
{
  extends: [ js.configs.recommended, typescriptEslint.configs.recommended ],
  files: ["test/**/*.ts"],
  ignores: ["test/fixtures/**/*"],
}]);
