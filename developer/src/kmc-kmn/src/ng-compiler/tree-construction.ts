/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";
import { NodeTypes } from "./node-types.js";

export class ASTNode {
  private _nodeType: NodeTypes;
  private _token: Token;
  private children: ASTNode[] = [];

  public constructor(nodeType: NodeTypes, token: Token=null) {
    this._nodeType = nodeType;
    this._token    = token;
  }

  public addChild(child: ASTNode): ASTNode {
    if (child !== null) {
      this.children.push(child);
    }
    return this;
  }

  public addChildren(children: ASTNode[]): ASTNode {
    if (children !== null) {
      this.children.push(...children);
    }
    return this;
  }

  public addNewChildWithToken(nodeType: NodeTypes, token: Token): ASTNode {
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

  public hasChildOfType(requiredType: NodeTypes): boolean  {
    for (const child of this.children) {
      if (child.nodeType === requiredType) {
        return true;
      }
    }
    return false;
  }

  public hasSoloChildOfType(requiredType: NodeTypes): boolean  {
    let count = 0;
    for (const child of this.children) {
      if (child.nodeType === requiredType) {
        count += 1;
      }
    }
    return count === 1;
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
    return Array(...this.children);
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

  public removeChildren(): ASTNode[] {
    const list: ASTNode[] = this.getChildren();
    this.children = [];
    return list;
  }

  public removeSoleChildOfType(requiredType: NodeTypes): ASTNode {
    const children: ASTNode[] = this.removeChildrenOfType(requiredType);
    return (children.length == 1) ? children[0] : null;
  }

  public removeChildrenOfType(requiredType: NodeTypes): ASTNode[] {
    return this.removeChildrenOfTypes([requiredType]);
  }

  public removeChildrenOfTypes(requiredTypes: NodeTypes[]): ASTNode[] {
    if (!requiredTypes?.length) {
      return [];
    }

    const list: ASTNode[]     = [];
    const children: ASTNode[] = [];

    for (const child of this.children) {
      if (requiredTypes.includes(child.nodeType)) {
        list.push(child);
      } else {
        children.push(child);
      }
    }

    this.children = children;
    return list;
  }

  public removeBlocks(parentType: NodeTypes, childType: NodeTypes): ASTNode[] {
    const blocks: ASTNode[]   = [];
    const children: ASTNode[] = [];
    let inParent: boolean     = false;
    let parent: ASTNode       = null;

    for (const child of this.children) {
      if (child.nodeType === parentType) {
        inParent = true;
        parent   = child;
        blocks.push(parent);
      } else if (inParent && child.nodeType === childType) {
        parent.addChild(child);
      } else {
        children.push(child);
      }
    }

    this.children = children;
    return blocks;
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

  public toText(): string {
    let text: string = ""
    const sourceCodeNode: ASTNode = this.getSoleChildOfType(NodeTypes.SOURCE_CODE);
    const lineNodes: ASTNode[]    = sourceCodeNode.getChildren();
    for (let lineNode of lineNodes) {
      text = text.concat(lineNode.token.line.toString())
    }
    return text;
  }
}
