/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Tree Construction)
 */

import { Token } from "./lexer.js";
import { NodeType } from "./node-type.js";

/**
 * ASTNode objects form the abstract syntax tree created by the recursive-
 * descent parser. The class provides methods used for building, testing
 * and using the AST to build the in-memory semantic model.
 */
export class ASTNode {
  /** the children of this node */
  private children: ASTNode[] = [];

  /**
   * Construct an ASTNode
   *
   * @param nodeType the node type
   * @param token a token from the input (or null)
   */
  public constructor(
    /** the node type */
    public readonly nodeType: NodeType,
    /** a token (if appropriate) from the input */
    public readonly token: Token=null
  ) {
  }

  /**
   * Add a child node (has no effect if child is null).
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
   * Add an array of child nodes (has no effect if array is null).
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
   * Add a new child node by type with optional token.
   *
   * @param nodeType the node type to add
   * @param token the token (or null)
   * @returns this
   */
  public addNewChildWithToken(nodeType: NodeType, token: Token=null): ASTNode {
    this.addChild(new ASTNode(nodeType, token));
    return this;
  }

  /**
   * Gather an array of all nodes in a tree of a given type.
   *
   * @param requiredType the required type
   * @returns an array of matching nodes (or an empty array)
   */
  public getDescendents(requiredType: NodeType) {
    const result: ASTNode[] = [];
    if (requiredType != null) {
      this.collectDescendents(result, requiredType);
    }
    return result;
  }

  /**
   * Recursively collect an array of all nodes in a tree of a given type.
   *
   * @param result an array used to accumulate matching nodes
   * @param requiredType the required type
   */
  private collectDescendents(result: ASTNode[], requiredType: NodeType): void {
    if (this.nodeType === requiredType)
      result.push(this);
    for (const child of this.children)
      child.collectDescendents(result, requiredType);
  }

  /**
   * The number of children this node has.
   *
   * @returns the number of children
   */
  public numberOfChildren(): number {
    return this.children.length;
  }

  /**
   * The number of children this node has of a given type.
   *
   * @param requiredType the required type
   * @returns the number of children of the required type
   */
  public numberOfChildrenOfType(requiredType: NodeType): number {
    return this.getChildrenOfType(requiredType).length;
  }

  /**
   * Check if this node has any children.
   *
   * @returns true if there is at least one child node
   */
  public hasChildren(): boolean {
    return this.children.length > 0;
  }

  /**
   * Check if this node has any children of a given type.
   *
   * @param requiredType the required type
   * @returns true if there is at least one child of the required type
   */
  public hasChildrenOfType(requiredType: NodeType): boolean  {
    return this.getChildrenOfType(requiredType).length > 0;
  }

  /**
   * Check if this node has one and only one child of a given type.
   *
   * @param requiredType the required type
   * @returns true if there is one and only one child of the required type
   */
  public hasSoleChildOfType(requiredType: NodeType): boolean  {
    return this.getChildrenOfType(requiredType).length == 1
  }

  /**
   * Get the text of the stored token, if any.
   *
   * @returns token text or an empty string
   */
  public getText(): String {
    return (this.token != null) ? this.token.text : '';
  }

  /**
   * Get the text of the stored token of a single child of the required type,
   * if there is one and only one such child.
   *
   * @param requiredType the required type
   * @returns token text or an empty string
   */
  public getTextOfType(requiredType: NodeType): String  {
    const child: ASTNode = this.getSoleChildOfType(requiredType);
    return (child != null) ? child.getText() : '';
  }

  /**
   * Get the sole child node, if there is one and only one.
   *
   * @returns the sole child node or null
   */
  public getSoleChild(): ASTNode {
    const children: ASTNode[] = this.getChildren();
    return (children.length == 1) ? children[0] : null;
  }

  /**
   * Get the sole child node of the required type, if there is one and only one.
   *
   * @returns the sole child node or null
   */
  public getSoleChildOfType(requiredType: NodeType): ASTNode {
    const children: ASTNode[] = this.getChildrenOfType(requiredType);
    return (children.length == 1) ? children[0] : null;
  }

  /**
   * Get an array of the child nodes.
   *
   * @returns the child nodes or an empty array
   */
  public getChildren(): ASTNode[] {
    return Array(...this.children);
  }

  /**
   * Get an array of the child nodes of the required type.
   *
   * @returns the child nodes or an empty array
   */
  public getChildrenOfType(requiredType: NodeType): ASTNode[] {
    return this.children.filter((child) => child.nodeType === requiredType);
  }

  /**
   * Remove and return all children as an array.
   *
   * @returns an array of child nodes
   */
  public removeChildren(): ASTNode[] {
    const list: ASTNode[] = this.getChildren();
    this.children = [];
    return list;
  }

  /**
   * Remove and return the first child node.
   *
   * @returns the first child node or null
   */
  public removeFirstChild(): ASTNode {
    return this.hasChildren() ? this.children.shift() : null;
  }

  /**
   * Remove and return the sole child node of the required type,
   * if there is one and only one.
   *
   * @returns the sole child of the required type or null
   */
  public removeSoleChildOfType(requiredType: NodeType): ASTNode {
    return this.hasSoleChildOfType(requiredType) ?
      this.removeChildrenOfType(requiredType)[0] : null;
  }

  /**
   * Remove and return all children of the required type as an array.
   *
   * @returns all children of the required type or an empty array
   */
  public removeChildrenOfType(requiredType: NodeType): ASTNode[] {
    return this.removeChildrenOfTypes([requiredType]);
  }

  /**
   * Remove and return all children of the required types as an array.
   *
   * @returns all children of the required types or an empty array
   */
  public removeChildrenOfTypes(requiredTypes: NodeType[]): ASTNode[] {
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

  /**
   * Removes all parent and child node types, assembling them into
   * an array of parent nodes, with all children found between one
   * parent and the next (or the end of the children) added to the
   * current parent. The array of parent nodes (with their newly
   * added children) are returned.
   *
   * @returns an array of parent trees or an empy array
   */
  public removeBlocks(parentType: NodeType, childType: NodeType): ASTNode[] {
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

  public toString(): string {
    let buf: string = `[${this.nodeType}`;
    if (this.token != null) {
      buf = buf.concat(`,${this.token}`);
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

  /**
   * Reassembles the source code file by concatenating
   * the token text of the children of the SOURCE_CODE node.
   *
   * @returns the source code
   */
  public toText(): string {
    let text: string = '';
    if (this.hasSoleChildOfType(NodeType.SOURCE_CODE)) {
      const sourceCodeNode = this.getSoleChildOfType(NodeType.SOURCE_CODE);
      for (const lineNode of sourceCodeNode.getChildren()) {
        text = text.concat(lineNode.token.line.toString())
      }
    }
    return text;
  }
}
