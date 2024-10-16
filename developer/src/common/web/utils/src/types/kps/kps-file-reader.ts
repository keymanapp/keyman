/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-10-16
 *
 * XML reader for .kps file format
 */

import { util } from '@keymanapp/common-types';
import boxXmlArray = util.boxXmlArray;

import { CommonTypesMessages } from "../../common-messages.js";
import { CompilerCallbacks } from "../../compiler-interfaces.js";
import { KeymanXMLReader } from "../../xml-utils.js";
import { KpsPackage } from "./kps-file.js";

export class KpsFileReader {
  constructor(private callbacks: CompilerCallbacks) {

  }

  public read(file: Uint8Array): KpsPackage {
    const data = new TextDecoder().decode(file);

    const kpsPackage = (() => {
        let a: KpsPackage;

        try {
          a = new KeymanXMLReader('kps')
            .parse(data) as KpsPackage;
        } catch(e) {
          this.callbacks.reportMessage(CommonTypesMessages.Error_InvalidPackageFile({e}));
        }
        return a;
    })();

    if(!kpsPackage) {
      return null;
    }

    return this.boxArrays(kpsPackage);
  }

  private boxArrays(data: KpsPackage): KpsPackage {
    boxXmlArray(data.Package?.Files, 'File');
    boxXmlArray(data.Package?.Keyboards, 'Keyboard');
    boxXmlArray(data.Package?.LexicalModels, 'LexicalModel');
    boxXmlArray(data.Package?.RelatedPackages, 'RelatedPackage');

    if(data.Package?.Keyboards?.Keyboard) {
      for(const k of data.Package.Keyboards.Keyboard) {
        boxXmlArray(k.Examples, 'Example');
        boxXmlArray(k.Languages, 'Language');
        boxXmlArray(k.WebDisplayFonts, 'Font');
        boxXmlArray(k.WebOSKFonts, 'Font');
      }
    }

    if(data.Package?.LexicalModels?.LexicalModel) {
      for(const lm of data.Package.LexicalModels.LexicalModel) {
        boxXmlArray(lm.Languages, 'Language');
      }
    }

    boxXmlArray(data.Package?.RelatedPackages, 'RelatedPackage');
    boxXmlArray(data.Package?.StartMenu?.Items, 'Item');
    boxXmlArray(data.Package?.Strings, 'String');

    return data;
  }
};
