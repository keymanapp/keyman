/**
 * A span that does not cut out the substring until it absolutely has to!
 */
export class LazySpan {
  private _source: string;
  readonly start: number;
  readonly end: number;
  constructor(source: string, start: number, end: number) {
    this._source = source;
    this.start = start;
    this.end = end;
  }

  get text(): string {
    return this._source.substring(this.start, this.end);
  }

  get length(): number {
    return this.end - this.start;
  }

  substring(start: number, end?: number): LazySpan {
    if(end === undefined) {
      end = start + this.length;
    }

    const maxIndex = this.start + this.length;
    end = end > maxIndex ? maxIndex : end;

    start = start < 0 ? 0 : start;
    if(end < start) {
      const temp = end;
      end = start;
      start = temp;
    }

    return new LazySpan(this._source, this.start + start, this.start + end);
  }
}