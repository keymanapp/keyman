import KEYMAN_VERSION from "@keymanapp/keyman-version";

// Dotted-decimal version
export default class Version {
  public static readonly CURRENT = new Version(KEYMAN_VERSION.VERSION_RELEASE);

  // Represents a default version value for keyboards compiled before this was compiled into keyboards.
  // The exact version is unknown at this point, but the value is "good enough" for what we need.
  public static readonly DEVELOPER_VERSION_FALLBACK = new Version([9, 0, 0]);

  // For 12.0, the old default behavior of adding missing keycaps to the default layers was removed,
  // as it results in unexpected, bug-like behavior for keyboard designers when it is unwanted.
  public static readonly NO_DEFAULT_KEYCAPS = new Version([12, 0]);

  public static readonly MAC_POSSIBLE_IPAD_ALIAS = new Version([10, 15]);

  private readonly components: number[]

  /**
   * Parses version information, preparing it for use in comparisons.
   * @param text Either a string representing a version number (ex: "9.0.0") or an array representing
   *             its components (ex: [9, 0, 0]).
   */
  constructor(text: String | number[]) {
    // If a keyboard doesn't specify a version, use the DEVELOPER_VERSION_FALLBACK values.
    if(text === undefined || text === null) {
      this.components = [].concat(Version.DEVELOPER_VERSION_FALLBACK.components);
      return;
    }

    if(Array.isArray(text)) {
      let components = text as number[];
      if(components.length < 2) {
        throw new Error("Version string must have at least a major and minor component!");
      } else {
        this.components = [].concat(components);
        return;
      }
    }

    // else, standard constructor path.
    let parts = text.split('.');
    let componentArray: number[] = [];

    if(parts.length < 2) {
      throw new Error("Version string must have at least a major and minor component!");
    }

    for(let i=0; i < parts.length; i++) {
      let value = parseInt(parts[i], 10);
      if(isNaN(value)) {
        throw new Error("Version string components must be numerical!");
      }

      componentArray.push(value);
    }

    this.components = componentArray;
  }

  get major(): number {
    return this.components[0];
  }

  get minor(): number {
    return this.components[1];
  }

  toString(): string {
    return this.components.join('.');
  }

  toJSON(): string {
    return this.toString();
  }

  equals(other: Version): boolean {
    return this.compareTo(other) == 0;
  }

  precedes(other: Version): boolean {
    return this.compareTo(other) < 0;
  }

  compareTo(other: Version): number {
    // If the version info depth differs, we need a flag to indicate which instance is shorter.
    var isShorter: boolean = this.components.length < other.components.length;
    var maxDepth: number = (this.components.length < other.components.length) ? this.components.length : other.components.length;

    var i: number;
    for(i = 0; i < maxDepth; i++) {
      let delta = this.components[i] - other.components[i];
      if(delta != 0) {
        return delta;
      }
    }

    var longList = isShorter ? other.components : this.components;
    do {
      if(longList[i] > 0) {
        return isShorter ? -1 : 1;
      }
      i++;
    } while (i < longList.length);

    // Equal.
    return 0;
  }
}