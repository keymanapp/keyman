/**
 * @file priority-queue.ts
 *
 * Defines a mildly abstracted priority queue implementation.
 */

namespace models {
  /**
   * Used to compare two instances of a type.
   * @returns 
   * - value < 0 if `a` should come before `b`
   * - value > 0 if `b` should come before `a`
   * - 0 if they should be treated equally.
   */
  export type Comparator<Type> = (a: Type, b: Type) => number;

  export class PriorityQueue<Type> {

    private comparator: Comparator<Type>;
    private heap: Type[] = [];

    /**
     * Constructs an empty priority queue.
     * @param comparator A `Comparator` returning negative values when and only when
     * the first parameter should precede the second parameter.
     */
    constructor(comparator: Comparator<Type>) {
      // TODO: We may wish to allow options specifying a limit or threshold for adding
      // items to the priority queue.  Possibly both.
      this.comparator = comparator;
    }

    private static leftChildIndex(index: number): number {
      return index * 2 + 1;
    }

    private static rightChildIndex(index: number): number {
      return index * 2 + 2;
    }

    private static parentIndex(index: number): number {
      return Math.floor((index-1)/2);
    }

    /**
     * Returns the number of elements currently held by the priority queue.
     */
    get count(): number {
      return this.heap.length;
    }

    /**
     * Returns the highest-priority item within the priority queue.
     * <p>
     * Is O(1).
     */
    peek() {
      return this.heap[0];  // undefined if it doesn't exist... which is completely correct.
    }

    /**
     * Inserts a new element into the priority queue, placing it in order.
     * <p>
     * Is O(log N), where N = # of items in the priority queue.
     * @param element 
     */
    enqueue(element: Type) {
      let index = this.heap.length;
      this.heap.push(element);

      let parent = PriorityQueue.parentIndex;
      let parentIndex = parent(index);
      while(index !== 0 && this.comparator(this.heap[index], this.heap[parentIndex]) < 0) {
        let a = this.heap[index];
        this.heap[index] = this.heap[parentIndex];
        this.heap[parentIndex] = a;

        index = parentIndex;
        parentIndex = parent(index);
      }
    }

    /**
     * Convenience function:  lazily calls enqueue for each element of the specified array.
     * @param elements 
     */
    enqueueAll(elements: Type[]) {
      for(let element of elements) {this.enqueue(element);
      }
    }

    /**
     * Removes the highest-priority element from the queue, returning it.
     * <p>
     * Is O(log N), where N = number of items in the priority queue.
     */
    dequeue(): Type {
      if(this.count == 0) {
        return undefined;
      }

      const root = this.heap[0];
      let tail = this.heap.pop();
      if(this.heap.length > 0) {
        this.heap[0] = tail;
        this.heapify(0);
      }

      return root;
    }

    /**
     * Compares the entry at the specified index against its children,
     * propagating it downward within the heap until heap requirements are specified.
     * <p>
     * Is O(log N), where N = number of items in the priority queue.
     * 
     * @param index The index of the top-most node that must be examined
     * for repositioning.
     */
    private heapify(index: number) {
      let leftIndex = PriorityQueue.leftChildIndex(index);
      let rightIndex = PriorityQueue.rightChildIndex(index);
      let topMostIndex = index;

      if(leftIndex < this.heap.length && this.comparator(this.heap[leftIndex], this.heap[topMostIndex]) < 0) {
        topMostIndex = leftIndex;
      }

      if(rightIndex < this.heap.length && this.comparator(this.heap[rightIndex], this.heap[topMostIndex]) < 0) {
        topMostIndex = rightIndex;
      }

      if(topMostIndex != index) {
        let a = this.heap[index];
        this.heap[index] = this.heap[topMostIndex];
        this.heap[topMostIndex] = a;

        this.heapify(topMostIndex);
      }
    }
  }
}