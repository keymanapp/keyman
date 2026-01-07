type DecodedCookieFieldValue = string | number | boolean;

type FilteredRecordEncoder = (value: DecodedCookieFieldValue, key: string) => string;
type FilteredRecordDecoder = (value: string, key: string) => DecodedCookieFieldValue;

export class CookieSerializer<Type extends Record<keyof Type, DecodedCookieFieldValue>> {
  private readonly name: string;

  public constructor(name: string) {
    this.name = name;
  }

  public load(decoder?: FilteredRecordDecoder): Type {
    return this.loadCookie(this.name, decoder || ((val: string) => val as DecodedCookieFieldValue)) as Type;
  }

  public save(cookie: Type, encoder?: FilteredRecordEncoder) {
    this.saveCookie(this.name, cookie, encoder || ((val: DecodedCookieFieldValue) => val as string));
  }

  /**
   * Document cookie parsing for use by kernel, OSK, UI etc.
   *
   * @return      {Object}                  array of names and strings
   */
  private _loadRawCookies(): Record<string, string> {
    const v: Record<string, string> = {};
    if(typeof(document.cookie) != 'undefined' && document.cookie != '') {
      const c = document.cookie.split(/;\s*/);
      for(let i = 0; i < c.length; i++) {
        const d = c[i].split('=');
        if(d.length == 2) {
          v[d[0]] = d[1];
        }
      }
    }

    return v;
  }

  /**
   * Document cookie parsing for use by kernel, OSK, UI etc.
   *
   * @param       {string}        cookieName        cookie name
   * @return      {Object}                  array of variables and values
   */
  private loadCookie(cookieName: string, decoder: FilteredRecordDecoder): Record<string, DecodedCookieFieldValue> {
    const cookie: Record<string, DecodedCookieFieldValue> = {};
    const allCookies = this._loadRawCookies();
    const encodedCookie = allCookies[cookieName];

    if(encodedCookie) {
      const rawDecode = decodeURIComponent(encodedCookie).split(';');
      for(let i=0; i<rawDecode.length; i++) {
        // Prevent accidental empty-key entries caused by cookie-final ';'.
        if(i == rawDecode.length - 1 && !rawDecode[i]) {
          break;
        }

        const record = rawDecode[i].split('=');
        if(record.length > 1) {
          const [key, value] = record;
          // key, value
          cookie[key] = decoder(value, key);
        } else {
          // key, <implied 'true', as boolean flag>
          cookie[record[0]] = '';
        }
      }
    }
    return cookie;
  }

  /**
   * Standard cookie saving for use by kernel, OSK, UI etc.
   *
   * @param       {string}      cookieName            name of cookie
   * @param       {Object}      cookieValueMap            object with array of named arguments and values
   */
  private saveCookie(cookieName: string, cookieValueMap: Record<string, DecodedCookieFieldValue>, encoder: FilteredRecordEncoder) {
    let serialization='';
    for(const key in cookieValueMap) {
      serialization += key + '=' + encoder(cookieValueMap[key], key) + ";";
    }

    const d = new Date(new Date().valueOf() + 1000 * 60 * 60 * 24 * 30).toUTCString();
    const cookieConfig = ' path=/; expires=' + d;  //Fri, 31 Dec 2099 23:59:59 GMT;';
    document.cookie = `${cookieName}=${encodeURIComponent(serialization)}; ${cookieConfig}`;
  }
}