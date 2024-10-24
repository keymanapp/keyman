type km_core_attr = import('./import/core/core-interface.js').km_core_attr;

export class CoreProcessor {
  private instance: any;

  /**
     * Initialize Core Processor
     * @param baseurl - The url where core.js is located
     */
  public async init(baseurl: string): Promise<boolean> {

    if (!this.instance) {
      try {
        const module = await import(baseurl + '/core.js');
        this.instance = await module.default({
          locateFile: function (path: string, scriptDirectory: string) {
            return baseurl + '/' + path;
          }
        });
      } catch (e: any) {
        return false;
      }
    }
    return !!this.instance;
  };

  public tmp_wasm_attributes(): km_core_attr {
    return this.instance.tmp_wasm_attributes();
  }
}
