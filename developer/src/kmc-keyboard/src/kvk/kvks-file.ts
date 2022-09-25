// .kvks xml format
// See VisualKeyboardLoaderXML.pas, not the same as VisualKeyboardImportXML.pas!

export default interface KVKSourceFile {
  /**
   * <visualkeyboard> -- the root element.
   */
  visualkeyboard: KVKSVisualKeyboard;
}

export interface KVKSVisualKeyboard {
  header?: KVKSHeader;
  encoding?: KVKSEncoding[];
};

export interface KVKSHeader {
  version?: string;
  kbdname?: string;
  flags?: KVKSFlags;
  keybitmap?: string;
  layout?: string;
};

export interface KVKSFlags {
  key102?: string;
  displayunderlying?: string;
  useunderlying?: string;
  usealtgr?: string;
};

export interface KVKSEncoding {
  name?: string;
  fontname?: string;
  fontsize?: string;
  layer?: KVKSLayer[];
};

export interface KVKSLayer {
  shift?: string;
  key?: KVKSKey[];
};

export interface KVKSKey {
  vkey?: string;
  bitmap?: string;
  _?: string;
};

/*

<?xml version="1.0" encoding="UTF-8"?>
<!ELEMENT visualkeyboard (header, encoding*)>
<!ELEMENT header (version, kbdname?, flags?, layout?)>
<!ELEMENT version (#PCDATA)>
<!ELEMENT kbdname (#PCDATA)>
<!ELEMENT flags (key102?, displayunderlying?, useunderlying?, usealtgr?)>
<!ELEMENT key102 EMPTY>
<!ELEMENT displayunderlying EMPTY>
<!ELEMENT useunderlying EMPTY>
<!ELEMENT usealtgr EMPTY>
<!ELEMENT layout (#PCDATA)>

<!ELEMENT encoding (layer*)>
<!ATTLIST encoding
  name CDATA #IMPLIED
  fontname CDATA #IMPLIED
  fontsize CDATA #IMPLIED
>

<!ELEMENT layer (key*)>
<!ATTLIST layer
  shift CDATA #IMPLIED
>

<!ELEMENT key (#PCDATA)>
<!ATTLIST key
  vkey CDATA #REQUIRED
  bitmap CDATA #IMPLIED
>


api.keyman.com visualkeyboard.dtd <-- note, this is NOT .kvks dTD
TODO-LDML: document .kvks XML schema

*/

