import { KvkFileReader, KvkFileWriter } from "@keymanapp/common-types";

export function setKvkFontData(fileData: Uint8Array, fontFacename: string) {
  const reader = new KvkFileReader();
  const kvk = reader.read(fileData);
  if(!kvk) {
    // kvk is invalid
    return null;
  }

  kvk.header.ansiFont.name = fontFacename;
  kvk.header.unicodeFont.name = fontFacename;

  const writer = new KvkFileWriter();
  return writer.write(kvk);
}