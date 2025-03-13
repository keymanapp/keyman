/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";

export enum NodeTypes {
  TMP,
}

export class ASTNode {
  private nodeType: NodeTypes;
  private token: Token;
  private children: ASTNode[] = [];

  public constructor(nodeType: NodeTypes, token: Token=null) {
    this.nodeType = nodeType;
    this.token    = token;
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

  public getChildren(): ASTNode[] {
    const list = [];
    for (const child of this.children) {
      list.push(child);
    }
    return list;
  }
}
