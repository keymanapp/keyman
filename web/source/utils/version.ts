namespace com.keyman.utils {
  // Dotted-decimal version
  export class Version {
    public static readonly NO_DEFAULT_KEYCAPS = new Version("12.0");

    private readonly components: number[]

    constructor(text: String) {
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

    equals(other: Version): boolean {
      if(this.components.length != other.components.length) {
        return false;
      } else {
        for(let i=0; i < this.components.length; i++) {
          if(this.components[i] != other.components[i]) {
            return false;
          }
        }

        return true;
      }
    }

    precedes(other: Version): boolean {
      // If the version info depth differs, we need a flag to indicate whether or not 'this' instance wins ties.
      // 12.0 is considered to precede 12.0.0.
      var tieBreaker: boolean = this.components.length < other.components.length;
      var maxDepth: number = tieBreaker ? this.components.length : other.components.length;

      for(let i=0; i < maxDepth; i++) {
        if(this.components[i] < other.components[i]) {
          return true;
        }
      }

      return tieBreaker;
    }

    static parseWithDefault(text: string, fallback: string): Version {
      if(!text) {
        return new Version(fallback);
      } else {
        return new Version(text);
      }
    }
  }
}