import EventEmitter from "eventemitter3";

interface LegacyAPIEvents {
  'kmw.keyboardregistered': (p: {} | {
    internalName: string,
    language: string,
    keyboardName: string,
    languageCode: string,
    package?: string
  }) => void;

  'kmw.keyboardloaded': (p: {
    keyboardName: string
  }) => void;
}

export class LegacyAPIEventEngine extends EventEmitter<LegacyAPIEvents> {
}