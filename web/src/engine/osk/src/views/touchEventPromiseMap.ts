import { ManagedPromise } from "@keymanapp/web-utils";

export default class TouchEventPromiseMap {
  private map: Map<number, ManagedPromise<void>> = new Map();

  // Used to
  public promiseForTouchpoint(id: number): ManagedPromise<void> {
    if(!this.map.get(id)) {
      this.map.set(id, new ManagedPromise<void>());
    }

    return this.map.get(id); // touchpoint identifiers are unique during a page's lifetime.
  }

  public maintainTouches(list: TouchList) {
    let keys = [].concat(this.map.keys());

    for(let i=0; i < list.length; i++) {
      let pos = keys.indexOf('' + list.item(i).identifier);
      if(pos != -1) {
        keys.splice(pos, 1);
      }
    }

    // Any remaining entries of `keys` are no longer in the map!
    for(let endedKey of keys) {
      this.map.get(endedKey).resolve();
      this.map.delete(endedKey);
    }
  }
}