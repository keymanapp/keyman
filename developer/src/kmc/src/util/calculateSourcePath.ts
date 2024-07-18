/**
 * This is a helper function to derive the relative path within the
 * @keymanapp/keyboards or @keymanapp/lexical-models repository, given a .kpj
 * project filename. Handles \ vs /. Must be a project filename, not a project
 * folder.
 *
 * For example, given "c/path/to/keyboards/release/k/keyboard/keyboard.kpj, we
 * want "release/k/keyboard".
 */
export function calculateSourcePath(projectFilename: string): string {
  projectFilename = projectFilename.replace(/\\/g, '/');
  const result = /(release|legacy|experimental)\/([^\/]+)\/([^\/]+)\/([^\/]+)\.kpj$/.exec(projectFilename);
  if (!result) {
    return undefined;
  }
  return `${result[1]}/${result[2]}/${result[3]}`;
}
