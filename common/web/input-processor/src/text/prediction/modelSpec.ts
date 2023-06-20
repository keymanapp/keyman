/**
 * Defines the metadata needed to support and load lexical models compatible with
 * Keyman Engine for Web.
 */
export default interface ModelSpec {
  /**
   * The model's unique identifier.
   */
  id: string;

  /**
   * The list of supported BCP-47 language codes.  Only one language should be supported,
   * although multiple variants based on region code (at min) may be specified.
   */
  languages: string[];

  /**
   * The path/URL to the file that defines the model.  If both `path` and `code` are specified,
   * `path` takes precedence.
   */
  path: string;

  /**
   * The raw JS script defining the model.  Only used if `path` is not specified.
   */
  code: string;
}