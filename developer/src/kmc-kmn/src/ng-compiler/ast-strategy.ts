/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-12-03
 *
 * KMC KMN Next Generation Parser (ASTStrategy)
 */

import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";

export abstract class ASTStrategy {
  public apply(node: ASTNode): ASTNode {
    return node;
  };
}

/**
 * An ASTStrategy that rebuilds the tree to be rooted at the given node
 */
export class GivenNode extends ASTStrategy {
  public constructor(
    /** the type of the node at which to root the tree */
    protected readonly nodeType: NodeType
  ) {
    super();
  }

  /**
   * Rebuilds the tree to be rooted at the given node or leaves
   * the tree unchanged if a node of the given type is not found
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the given node
   */
  public apply(node: ASTNode): ASTNode {
    if (node.hasSoleChildOfType(this.nodeType)) {
      const givenNode: ASTNode = node.removeSoleChildOfType(this.nodeType);
      givenNode.addChildren(node.removeChildren());
      node.addChild(givenNode);
    }
    return node;
  };
}

/**
 * An ASTStrategy that rebuilds the tree to be rooted at a new node
 */
export class NewNode extends ASTStrategy {
  public constructor(
    /** the type of the new node at which to root the tree */
    protected readonly nodeType: NodeType
  ) {
    super();
  }

  /**
   * Rebuilds the tree to be rooted at a new node
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the new node
   */
  public apply(node: ASTNode): ASTNode {
    const newNode: ASTNode = new ASTNode(this.nodeType);
    newNode.addChildren(node.removeChildren());
    node.addChild(newNode)
    return node;
  };
}