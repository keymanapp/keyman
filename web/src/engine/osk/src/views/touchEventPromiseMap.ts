import { ManagedPromise } from "@keymanapp/web-utils";

export default class TouchEventPromiseMap {
  private map: Record<number, ManagedPromise<void>> = {};

  // Used to
  public promiseForTouchpoint(id: number): ManagedPromise<void> {
    if(!this.map[id]) {
      this.map[id] = new ManagedPromise<void>();
    }

    return this.map[id]; // touchpoint identifiers are unique during a page's lifetime.
  }

  public maintainTouches(list: TouchList) {
    let keys = Object.keys(this.map);

    for(let i=0; i < list.length; i++) {
      let pos = keys.indexOf('' + list.item(i).identifier);
      if(pos != -1) {
        keys.splice(pos, 1);
      }
    }

    // Any remaining entries of `keys` are no longer in the map!
    for(let endedKey of keys) {
      (this.map[endedKey] as ManagedPromise<void>).resolve();
      delete this.map[endedKey];
    }
  }
}