/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";

export enum NodeTypes {
  ANSI                  = "ANSI",
  ANY                   = "ANY",
  BASELAYOUT            = "BASELAYOUT",
  BASELAYOUT_SHORTCUT   = "BASELAYOUT_SHORTCUT",
  BEEP                  = "BEEP",
  BEGIN                 = "BEGIN",
  BITMAP                = "BITMAP",
  CALL                  = "CALL",
  CAPS                  = "CAPS",
  CAPSALWAYSOFF         = "CAPSALWAYSOFF",
  CAPSONONLY            = "CAPSONONLY",
  CASEDKEYS             = "CASEDKEYS",
  CONTEXT               = "CONTEXT",
  COPYRIGHT             = "COPYRIGHT",
  DEADKEY               = "DEADKEY",
  DECIMAL               = "DECIMAL",
  DISPLAYMAP            = "DISPLAYMAP",
  EQUAL                 = "EQUAL",
  ETHNOLOGUECODE        = "ETHNOLOGUECODE",
  GROUP                 = "GROUP",
  GROUPNAME             = "GROUPNAME",
  HEXADECIMAL           = "HEXADECIMAL",
  HOTKEY                = "HOTKEY",
  IF                    = "IF",
  INCLUDECODES          = "INCLUDECODES",
  INDEX                 = "INDEX",
  INPUT_CONTEXT         = "INPUT_CONTEXT",
  KEY_CODE              = "KEY_CODE",
  KEYBOARDVERSION       = "KEYBOARDVERSION",
  KEYSTROKE             = "KEYSTROKE",
  KMW_EMBEDCSS          = "KMW_EMBEDCSS",
  KMW_EMBEDJS           = "KMW_EMBEDJS",
  KMW_HELPFILE          = "KMW_HELPFILE",
  KMW_HELPTEXT          = "KMW_HELPTYEXT",
  KMW_RTL               = "KMW_RTL",
  LANGUAGE              = "LANGUAGE",
  LAYER                 = "LAYER",
  LAYER_SHORTCUT        = "LAYER_SHORTCUT",
  LAYOUTFILE            = "LAYOUTFILE",
  LHS_CONTEXT           = "LHS_CONTEXT",
  LHS_READONLY          = "LHS_READONLY",
  LHS_USING_KEYS        = "LHS_USING_KEYS",
  LINE                  = "LINE",
  MATCH                 = "MATCH",
  MESSAGE               = "MESSAGE",
  MNEMONICLAYOUT        = "MNEMONICLAYOUT",
  MODIFIER              = "MODIFIER",
  NAME                  = "NAME",
  NEWCONTEXT            = "NEWCONTEXT",
  NEWLAYER              = "NEWLAYER",
  NOMATCH               = "NOMATCH",
  NOT_EQUAL             = "NOT_EQUAL",
  NOTANY                = "NOTANY",
  NUL                   = "NUL",
  OCTAL                 = "OCTAL",
  OFFSET                = "OFFSET",
  OLDLAYER              = "OLDLAYER",
  OUTS                  = "OUTS",
  PARAMETER             = "PARAMETER",
  PLATFORM              = "PLATFORM",
  PLATFORM_SHORTCUT     = "PLATFORM_SHORTCUT",
  POSTKEYSTROKE         = "POSTKEYSTROKE",
  PRODUCTION_CONTEXT    = "PRODUCTION_CONTEXT",
  PRODUCTION_READONLY   = "PRODUCTION_READONLY",
  PRODUCTION_USING_KEYS = "PRODUCTION_USING_KEYS",
  RANGE                 = "RANGE",
  READONLY              = "READONLY",
  RESET                 = "RESET",
  RETURN                = "RETURN",
  RHS                   = "RHS",
  SAVE                  = "SAVE",
  SET                   = "SET",
  SHIFT                 = "SHIFT",
  SHIFTFREESCAPS        = "SHIFTFREESCAPS",
  STORE                 = "STORE",
  STORENAME             = "STORENAME",
  STRING                = "STRING",
  TARGETS               = "TARGETS",
  TMP                   = "TMP",
  U_CHAR                = "U_CHAR",
  UNICODE               = "UNICODE",
  USE                   = "USE",
  USING_KEYS            = "USING_KEYS",
  VERSION               = "VERSION",
  VIRTUAL_KEY           = "VIRTUAL_KEY",
  VISUALKEYBOARD        = "VISUALKEYBOARD",
  WINDOWSLANGUAGES      = "WINDOWSLANGUAGES",
}

export class ASTNode {
  private _nodeType: NodeTypes;
  private _token: Token;
  private children: ASTNode[] = [];

  public constructor(nodeType: NodeTypes, token: Token=null) {
    this._nodeType = nodeType;
    this._token     = token;
  }

  public addChild(child: ASTNode): ASTNode {
    if (child !== null) {
      this.children.push(child);
    }
    return this;
  }

  public addChildren(children: ASTNode[]): ASTNode {
    if (children !== null) {
      for (const child of children) {
        this.addChild(child);
      }
    }
    return this;
  }

  public addToken(nodeType: NodeTypes, token: Token): ASTNode {
    this.addChild(new ASTNode(nodeType, token));
    return this;
  }

  public getDescendents(requiredType: NodeTypes) {
    const result: ASTNode[] = [];
    this.collectDescendents(result, requiredType);
    return result;
  }

  private collectDescendents(result: ASTNode[], requiredType: NodeTypes): void {
    if (this.nodeType === requiredType)
      result.push(this);
    for (const child of this.children)
      child.collectDescendents(result, requiredType);
  }

  public hasChild(): boolean {
    return this.children.length > 0;
  }

  public hasChildOfType(nodeType: NodeTypes): boolean  {
    return this.getChildrenOfType(nodeType).length > 0;
  }

  public getText(): String {
    return (this._token !== null) ? this._token.text : '';
  }

  public getTextOfType(nodeType: NodeTypes): String  {
    const child: ASTNode = this.getSoleChildOfType(nodeType);
    return (child !== null) ? child.getText() : '';
  }

  public getSoleChild(): ASTNode {
    const children: ASTNode[] = this.getChildren();
    return (children.length == 1) ? children[0] : null;
  }

  public getSoleChildOfType(requiredType: NodeTypes): ASTNode {
    const children: ASTNode[] = this.getChildrenOfType(requiredType);
    return (children.length == 1) ? children[0] : null;
  }

  public getChildren(): ASTNode[] {
    const list: ASTNode[] = [];
    list.push(...this.children);
    return list;
  }

  public getChildrenOfType(requiredType: NodeTypes): ASTNode[] {
    const list: ASTNode[] = [];
    for (const child of this.children) {
      if (child.nodeType === requiredType) {
        list.push(child);
      }
    }
    return list;
  }

  public removeSoleChildOfType(requiredType: NodeTypes): ASTNode {
    const children: ASTNode[] = this.removeChildrenOfType(requiredType);
    return (children.length == 1) ? children[0] : null;
  }

  public removeChildrenOfType(requiredType: NodeTypes): ASTNode[] {
    const list: ASTNode[] = [];
    this.children.forEach((child, idx) => {
      if (child.nodeType === requiredType) {
        list.push(child);
        this.children.splice(idx, 1);
      }
    });
    return list;
  }

  public get nodeType() { return this._nodeType; }
  public get token() { return this._token; }

  public toString(): string {
    let buf: string = `[${this._nodeType}`;
    if (this._token !== null) {
      buf = buf.concat(`,${this._token}`);
    }
    if (this.children.length > 0) {
      buf = buf.concat(',{');
    }
    this.children.forEach((child, idx) => {
      buf = buf.concat(child.toString());
      if (idx < (this.children.length-1)) {
        buf = buf.concat(',');
        if (child._nodeType === NodeTypes.LINE) {
          buf = buf.concat('\n');
        }
      }
    });
    if (this.children.length > 0) {
      buf = buf.concat('}');
    }
    buf = buf.concat(']');
    return buf;
  }
}
