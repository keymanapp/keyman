/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KeymanXMLReader } from '../../index.js'
import { RegressionTestSourceFile, RegTestEvent, RegTestShiftState } from './regression-test-file.js';

export class RegressionTestSourceFileReader {
  public static isRegressionTestFile(file: Uint8Array) {
    const data = new TextDecoder().decode(file);
    return data.indexOf('<regressiontest') >= 0;
  }

  public read(file: Uint8Array): RegressionTestSourceFile {
    const source = new KeymanXMLReader('regressiontest')
      .parse(new TextDecoder().decode(file));
    if(source) {
      return this.transform(source);
    }
    return source;
  }

  private findNode(node: any, name: string): any {
    if(!Array.isArray(node)) {
      return null;
    }
    for(const item of node) {
      if(item[name]) {
        return item[name];
      }
    }
    return null;
  }

  private transform(source: any): RegressionTestSourceFile {
    // The source format is quite verbose due to the original file having
    // non-tree format. We'll convert into a tree format for use
    const root = this.findNode(source, 'regressiontest');
    const info = this.findNode(root, 'info');
    const events = this.findNode(root, 'events');

    if(!info || !events) return null;
    if(!events || !Array.isArray(events)) return null;
    const result: RegressionTestSourceFile = {
      info: {
        version: this.findNode(info, 'version')?.[0]?._,
        systemkeyboard: this.findNode(info, 'systemkeyboard')?.[0]?._,
        keyboard: this.findNode(info, 'keyboard')?.[0]?._?.replaceAll(/\\/g, '/'),
        beginmode: this.findNode(info, 'beginmode')?.[0]?._,
      },
      events: []
    };

    for(const event of events) {
      const key = this.findNode(event?.event, 'key');
      if(!key) return null;
      const eventResult: RegTestEvent = {
        key: {
          vkey: this.findNode(key, 'vkey')?.[0]?._,
          shiftstate: this.readShiftState(this.findNode(key, 'shiftstate'))
        },
        postcontext: [

        ]
      }
      const pc = this.findNode(event?.event, 'postcontext');
      if(!pc) return null;
      for(const c of pc) {
        if(c.text) {
          eventResult.postcontext.push(c.text?.[0]?._);
        } else if(c.deadkey) {
          eventResult.postcontext.push({deadkey:c.deadkey?.[0]?._});
        }
      }
      result.events.push(eventResult);
    }

    return result;
  }

  private readShiftState(shiftState: any): RegTestShiftState[] {
    const result: RegTestShiftState[] = [];
    if(this.findNode(shiftState, 'shift')) result.push(RegTestShiftState.shift);
    if(this.findNode(shiftState, 'ctrl')) result.push(RegTestShiftState.ctrl);
    if(this.findNode(shiftState, 'rctrl')) result.push(RegTestShiftState.rctrl);
    if(this.findNode(shiftState, 'alt')) result.push(RegTestShiftState.alt);
    if(this.findNode(shiftState, 'altgr')) result.push(RegTestShiftState.altgr);
    if(this.findNode(shiftState, 'caps')) result.push(RegTestShiftState.caps);
    return result;
  }
}
