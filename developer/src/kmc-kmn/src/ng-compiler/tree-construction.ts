/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";

export enum NodeTypes {
  BITMAP,
  STRING,
  TMP,
}

export class ASTNode {
  private _nodeType: NodeTypes;
  private token: Token;
  private children: ASTNode[] = [];

  public constructor(nodeType: NodeTypes, token: Token=null) {
    this._nodeType = nodeType;
    this.token     = token;
  }

  public addChild(child: ASTNode): void {
    this.children.push(child);
  }

  public addChildren(children: ASTNode[]): void {
    for (const child of children) {
      this.addChild(child);
    }
  }

  public addToken(nodeType: NodeTypes, token: Token): void {
    this.addChild(new ASTNode(nodeType, token));
  }

  public getDescendents(requiredType: NodeTypes) {
    const result: ASTNode[] = [];
    this.collectDescendents(result, requiredType);
    return result;
  }

  private collectDescendents(result: ASTNode[], requiredType: NodeTypes): void {
    if (this.nodeType == requiredType)
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
    return this.token.text;
  }

  public getTextOfType(nodeType: NodeTypes): String  {
    return this.getSoleChildOfType(nodeType).getText();
  }

  public getSoleChild(): ASTNode {
    const children: ASTNode[] = this.getChildren();
    return children[0];
  }

  public getSoleChildOfType(requiredType: NodeTypes): ASTNode {
    const children: ASTNode[] = this.getChildrenOfType(requiredType);
    return children[0];
  }

  public getChildren(): ASTNode[] {
    const list = [];
    for (const child of this.children) {
      list.push(child);
    }
    return list;
  }

  public getChildrenOfType(requiredType: NodeTypes): ASTNode[] {
    const list = [];
    for (const child of this.children) {
      if (child.nodeType === requiredType) {
        list.push(child);
      }
    }
    return list;
  }

  public get nodeType() { return this._nodeType; }
}
