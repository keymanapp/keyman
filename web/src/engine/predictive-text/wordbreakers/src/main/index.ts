import { placeholder } from "./placeholder.js";
import { ascii } from "./ascii.js";
import { default_, LazySpan } from "./default/index.js";
import { WordBreakProperty } from "./default/data.inc.js";
import { searchForProperty } from "./default/searchForProperty.js";
import { sanitizeResults } from "./sanitize-results.js";

export {
  placeholder,
  ascii,
  default_ as default,
  default_ as defaultWordbreaker,
  LazySpan,
  sanitizeResults,
  searchForProperty,
  WordBreakProperty
};

export { type BreakerContext } from "./default/index.js";
