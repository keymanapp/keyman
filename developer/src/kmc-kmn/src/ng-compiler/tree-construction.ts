/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";
import { NodeTypes } from "./node-types.js";

/**
 * ASTNode objects form the abstract syntax tree created by the recursive-
 * descent parser. The class provides methods used for building, testing
 * and using the AST to build the in-memory semantic model.
 */
export class ASTNode {
  private _nodeType: NodeTypes; // the node type
  private _token: Token; // a token (if appropriate) from the input
  private children: ASTNode[] = [];

  /**
   * Construct an ASTNode
   *
   * @param nodeType the node type
   * @param token a token from the input (or null)
   */
  public constructor(nodeType: NodeTypes, token: Token=null) {
    this._nodeType = nodeType;
    this._token    = token;
  }

  /**
   * Add a child node
   *
   * @param child the node to add
   * @returns this
   */
  public addChild(child: ASTNode): ASTNode {
    if (child != null) {
      this.children.push(child);
    }
    return this;
  }

  /**
   * Add an array of child nodes
   *
   * @param children the array of nodes to add
   * @returns this
   */
  public addChildren(children: ASTNode[]): ASTNode {
    if (children != null) {
      this.children.push(...children);
    }
    return this;
  }

  /**
   * Add a new child node by type with optional token
   *
   * @param nodeType the node type to add
   * @param token the token (or null)
   * @returns this
   */
  public addNewChildWithToken(nodeType: NodeTypes, token: Token=null): ASTNode {
    this.addChild(new ASTNode(nodeType, token));
    return this;
  }

  /**
   * Gather an array of all nodes in a tree of a given type
   *
   * @param requiredType the required type
   * @returns an array of matching nodes (or an empty array)
   */
  public getDescendents(requiredType: NodeTypes) {
    const result: ASTNode[] = [];
    if (requiredType != null) {
      this.collectDescendents(result, requiredType);
    }
    return result;
  }

  private collectDescendents(result: ASTNode[], requiredType: NodeTypes): void {
    if (this.nodeType === requiredType)
      result.push(this);
    for (const child of this.children)
      child.collectDescendents(result, requiredType);
  }

  /**
   * Check if this node has any children
   *
   * @returns true if there is at least one child node
   */
  public hasChild(): boolean {
    return this.children.length > 0;
  }

  /**
   * Check if this node has any children of a given type
   *
   * @param requiredType the required type
   * @returns true if there is at least one child of the required type
   */
  public hasChildOfType(requiredType: NodeTypes): boolean  {
    if (requiredType != null) {
      for (const child of this.children) {
        if (child.nodeType === requiredType) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Check if this node has one and only one child of a given type
   *
   * @param requiredType the required type
   * @returns true if there is one and only one child of the required type
   */
  public hasSoleChildOfType(requiredType: NodeTypes): boolean  {
    let count = 0;
    if (requiredType != null) {
      for (const child of this.children) {
        if (child.nodeType === requiredType) {
          count += 1;
        }
      }
    }
    return count === 1;
  }

  public getText(): String {
    return (this._token != null) ? this._token.text : '';
  }

  public getTextOfType(requiredType: NodeTypes): String  {
    const child: ASTNode = this.getSoleChildOfType(requiredType);
    return (child != null) ? child.getText() : '';
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
    if (requiredType != null) {
      for (const child of this.children) {
        if (child.nodeType === requiredType) {
          list.push(child);
        }
      }
    }
    return list;
  }

  public removeChildren(): ASTNode[] {
    const list: ASTNode[] = this.getChildren();
    this.children = [];
    return list;
  }

  public removeFirstChild(): ASTNode {
    return this.hasChild() ? this.children.shift() : null;
  }

  public removeSoleChildOfType(requiredType: NodeTypes): ASTNode {
    return this.hasSoleChildOfType(requiredType) ?
      this.removeChildrenOfType(requiredType)[0] : null;
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

    if (parentType == null || childType == null) {
      return [];
    }

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
    if (this._token != null) {
      buf = buf.concat(`,${this._token}`);
    }
    if (this.children.length > 0) {
      buf = buf.concat(',{');
    }
    this.children.forEach((child, idx) => {
      buf = buf.concat(child.toString());
      if (idx < (this.children.length-1)) {
        buf = buf.concat(',');
      }
    });
    if (this.children.length > 0) {
      buf = buf.concat('}');
    }
    buf = buf.concat(']');
    return buf;
  }

  public toText(): string {
    let text: string = '';
    if (this.hasSoleChildOfType(NodeTypes.SOURCE_CODE)) {
      const sourceCodeNode = this.getSoleChildOfType(NodeTypes.SOURCE_CODE);
      for (const lineNode of sourceCodeNode.getChildren()) {
        text = text.concat(lineNode.token.line.toString())
      }
    }
    return text;
  }
}
