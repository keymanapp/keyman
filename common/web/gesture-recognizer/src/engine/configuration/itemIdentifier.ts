import { InputSample } from "../headless/inputSample.js";
import { CumulativePathStats } from "../index.js";

export type BaseItemIdentifier<Type> = (coord: Omit<InputSample<any>, 'item'>, target: EventTarget) => Type;
export type GestureItemIdentifier<Type> = (baseItem: Type, currentItem: Type, stats: CumulativePathStats) => Type;