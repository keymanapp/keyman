import { KeySpec, Keys } from './keys.js';
import { Strs } from './strs.js';
import { decodeNumber } from './utils.js';

export type Layer = {
  id: string;
  modifiers: number;
  keys: KeySpec[][];
};

export type LayerList = {
  layers: Layer[],
  minDeviceWidth: number,
  name: string
};

export class Layr {
  readonly name = 'layr';
  readonly data: Uint8Array;
  readonly layerLists: Readonly<LayerList>[]

  constructor(rawData: Uint8Array, strs: Strs, keys: Keys) {
    this.data = rawData;

    const layerListCount = decodeNumber(rawData, 8);

    const layerListTableOffset = 24;
    const LAYER_LIST_ENTRY_SIZE = 16;
    const layerTableOffset = layerListTableOffset + LAYER_LIST_ENTRY_SIZE * layerListCount;

    const layers = this.processLayers(layerTableOffset, strs, keys);

    const layerLists: LayerList[] = [];

    for(let i = 0; i < layerListCount; i++) {
      const rowOffset = layerListTableOffset + i * LAYER_LIST_ENTRY_SIZE;

      const name = strs.entries[decodeNumber(rawData, rowOffset)];
      const firstLayerIndex = decodeNumber(rawData, rowOffset + 4);
      const layerCount = decodeNumber(rawData, rowOffset + 8);
      const minDeviceWidth = decodeNumber(rawData, rowOffset + 12);

      const listLayers: Layer[] = [];
      for(let j = 0; j < layerCount; j++) {
        listLayers.push(layers[firstLayerIndex + j]);
      }

      layerLists.push({
        name, minDeviceWidth, layers: listLayers
      });
    }

    this.layerLists = layerLists;
  }

  private processLayers(layerTableOffset: number, strs: Strs, keys: Keys) {
    const data = this.data;

    const layerCount = decodeNumber(data, 12);
    const LAYER_ENTRY_SIZE = 16;

    const rowCount = decodeNumber(data, 16);
    const ROW_ENTRY_SIZE = 8;

    // const keyCount = decodeNumber(data, 20);
    const KEY_ENTRY_SIZE = 4;

    const rowTableOffset = layerTableOffset + LAYER_ENTRY_SIZE * layerCount;
    const keyTableOffset = rowTableOffset + ROW_ENTRY_SIZE * rowCount;

    const layers: Layer[] = [];
    for(let i = 0; i < layerCount; i++) {
      const layerOffset = layerTableOffset + i * LAYER_ENTRY_SIZE;

      const id = strs.entries[decodeNumber(data, layerOffset)];
      const modifiers = decodeNumber(data, layerOffset + 4);
      const firstRowIndex = decodeNumber(data, layerOffset + 8);
      const rowCount = decodeNumber(data, layerOffset + 12);

      const baseRowOffset = rowTableOffset + firstRowIndex * ROW_ENTRY_SIZE;
      const rows: KeySpec[][] = []; // TODO:
      for(let j = 0; j < rowCount; j++) {
        const rowOffset = baseRowOffset + j * ROW_ENTRY_SIZE;

        const firstKeyIndex = decodeNumber(data, rowOffset);
        const keyCount = decodeNumber(data, rowOffset + 4);

        const baseKeyOffset = keyTableOffset + firstKeyIndex * KEY_ENTRY_SIZE;
        const rowKeys: KeySpec[] = [];
        for(let k = 0; k < keyCount; k++) {
          const keyId = strs.entries[decodeNumber(data, baseKeyOffset + 4 * k)];
          rowKeys.push(keys.keys.get(keyId));
        }

        rows.push(rowKeys);
      }

      layers.push({
        id, modifiers, keys: rows
      });
    }

    return layers;
  }
}