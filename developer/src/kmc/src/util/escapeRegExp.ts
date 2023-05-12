
const escapedRegexp = /[.*+?^${}()|[\]\\]/g;

// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping
export function escapeRegExp(s: string) {
  return s.replace(escapedRegexp, "\\$&"); // $& means the whole matched string
}
