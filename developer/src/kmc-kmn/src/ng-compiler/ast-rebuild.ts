/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-12-03
 *
 * KMC KMN Next Generation Parser (ASTRebuild)
 */

import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";

/**
 * An abstract strategy for rebuilding an AST
 */
export abstract class ASTRebuild {
  public abstract apply(node: ASTNode): ASTNode;
}

/**
 * Rebuilds the tree to be rooted at the given node
 */
export class GivenNode extends ASTRebuild {
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
 * Rebuilds the tree as given parent and child nodes
 */
export class StackedPair extends ASTRebuild {
  public constructor(
    /** the type of the node at which to root the tree */
    protected readonly parentType: NodeType,
    /** the type of the child node */
    protected readonly childType: NodeType
  ) {
    super();
  }

  /**
   * Rebuilds the tree to consist of the given parent and child nodes
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the given parent node
   */
  public apply(node: ASTNode): ASTNode {
    if (node.numberOfChildren() == 2 &&
        node.hasSoleChildOfType(this.parentType) &&
        node.hasSoleChildOfType(this.childType)) {
      const parentNode = node.removeSoleChildOfType(this.parentType);
      const childNode  = node.removeSoleChildOfType(this.childType);
      parentNode.addChild(childNode);
      node.addChild(parentNode);
    }
    return node;
  };
}

/**
 * Rebuilds the tree to be rooted at a new node
 */
export class NewNode extends ASTRebuild {
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
    node.addChild(newNode);
    return node;
  };
}

/**
 * Adds either a single new node or builds a tree rooted at a new node,
 * depending on the number of children found (one or more)
 */
export class NewNodeOrTree extends ASTRebuild {
  public constructor(
    /** the type of the new node at which to root the tree */
    protected readonly nodeType: NodeType
  ) {
    super();
  }
  /**
   * Adds a single new node of the given type (for one child found) or
   * rearranges the tree to be rooted at a new node of the given type
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the first node found
   */
  public apply(node: ASTNode): ASTNode {
    const children = node.removeChildren();
    // two structures depending on number of children
    if (children.length === 1) {
      node.addNewChildWithToken(this.nodeType, children[0].token);
    } else {
      const newNode: ASTNode = new ASTNode(this.nodeType);
      newNode.addChildren(children);
      node.addChild(newNode);
    }
    return node;
  };
}

/**
 * Rebuilds the tree to be rooted at the first node found
 */
export class FirstNode extends ASTRebuild {
  /**
   * Rebuilds the tree to be rooted at the first node found
   * or leaves the tree unchanged if there are no children
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the first node found
   */
  public apply(node: ASTNode): ASTNode {
    if (node.hasChildren()) {
      const firstNode: ASTNode = node.removeFirstChild();
      firstNode.addChildren(node.removeChildren());
      node.addChild(firstNode);
    }
    return node;
  };
}

/**
 * Replaces the sole child node with a new node
 * of given type but retaining the same Token
 */
export class ChangeNode extends ASTRebuild {
  public constructor(
    /** the new node type */
    protected readonly nodeType: NodeType
  ) {
    super();
  }

  /**
   * Replaces the sole child node with a new node
   * of given type but retaining the same Token
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree
   */
  public apply(node: ASTNode): ASTNode {
    if (node.hasSoleChild()) {
      const oldNode    = node.removeSoleChild();
      const changeNode = new ASTNode(this.nodeType, oldNode.token);
      node.addChild(changeNode);
    }
    return node;
  };
}