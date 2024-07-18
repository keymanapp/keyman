/**
 * @file priority-queue.ts
 *
 * Defines a mildly abstracted priority queue implementation.
 */

/**
 * Used to compare two instances of a type.
 * @returns
 * - value < 0 if `a` should come before `b`
 * - value > 0 if `b` should come before `a`
 * - 0 if they should be treated equally.
 */
export type Comparator<Type> = (a: Type, b: Type) => number;

export default class PriorityQueue<Type> {

  private comparator: Comparator<Type>;
  private heap: Type[];

  /**
   * Shallow-copy / clone constructor.
   * @param instance
   */
  constructor(instance: PriorityQueue<Type>);
  /**
   * Constructs an empty priority queue.
   * @param comparator A `Comparator` returning negative values when and only when
   * the first parameter should precede the second parameter.
   * @param initialEntries
   */
  constructor(comparator: Comparator<Type>, initialEntries?: Type[]);
  constructor(arg1: Comparator<Type> | PriorityQueue<Type>, initialEntries?: Type[]) {
    if(typeof arg1 != 'function') {
      this.comparator = arg1.comparator;
      // Shallow-copies are fine.
      this.heap = ([] as Type[]).concat(arg1.heap);
      return;
    }

    const comparator = arg1;
    // TODO: We may wish to allow options specifying a limit or threshold for adding
    // items to the priority queue.  Possibly both.
    //
    // When that time comes, consider a min-max heap.
    // https://en.wikipedia.org/wiki/Min-max_heap
    this.comparator = comparator;

    this.heap = (initialEntries ?? []).slice(0);
    this.heapify();
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
   * Maintains internal state, rearranging the internal state until all heap constraints
   * are properly satisfied.
   * - O(N) when 'heapifying' the whole heap
   * - O(N) worst-case for partial heap operations (as part of an enqueueAll)
   * <p>
   */
  private heapify(): void;
  private heapify(start: number, end: number): void;
  private heapify(start?: number, end?: number): void {
    if(start == undefined || end == undefined) {
      this.heapify(0, this.count - 1);
      return;
    }

    // Use of 'indices' here is a bit of a customization.
    // At the cost of (temporary) extra storage space, we can more efficiently enqueue
    // multiple elements simultaneously.
    let queuedIndices: number[] = [];
    let lastParent = -1;

    for(let i = end; i >= start; i--) {
      let parent = PriorityQueue.parentIndex(i);
      if(this.siftDown(i) && parent < start && lastParent != parent) {
        // We only need to queue examination for a heap node if its children have changed
        // and it isn't already being examined.
        queuedIndices.push(parent);
        lastParent = parent;
      }
    }

    lastParent = -1;
    while(queuedIndices.length > 0) {
      let index = queuedIndices.shift() as number;
      let parent = PriorityQueue.parentIndex(index);
      if(this.siftDown(index) && parent >= 0 && lastParent != parent) {
          // We only need to queue examination for a heap node if its children have changed.
        queuedIndices.push(parent);
        lastParent = parent;
      }
    }
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
   * Efficiently batch-enqueues multiple elements.
   * Worst-case is the _better_ of the following:
   * - O(`elements.count` + `heap.count`) - large element counts will trigger in-place
   * heap reconstruction.
   * - O(`elements.count` * log(`heap.count`)) - logarithmic when elements.count << heap.count
   * @param elements A group of elements to enqueue simultaneously.
   */
  enqueueAll(elements: Type[]) {
    if(elements.length == 0) {
      return;
    }

    let firstIndex = this.count
    this.heap = this.heap.concat(elements);
    let firstParent = PriorityQueue.parentIndex(firstIndex);

    // The 'parent' of index 0 will return -1, which is illegal.
    this.heapify(firstParent >= 0 ? firstParent : 0, PriorityQueue.parentIndex(this.count-1));
  }

  /**
   * Removes the highest-priority element from the queue, returning it.
   * <p>
   * Is O(log N), where N = number of items in the priority queue.
   */
  dequeue(): Type | undefined {
    if(this.count == 0) {
      return undefined;
    }

    const root = this.heap[0];
    let tail = this.heap.pop() as Type;
    if(this.heap.length > 0) {
      this.heap[0] = tail;
      this.siftDown(0);
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
   * @returns `true` if a swap occurred, `false` otherwise.
   */
  private siftDown(index: number): boolean {
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

      this.siftDown(topMostIndex);
      return true;
    } else {
      return false;
    }
  }

  /**
   * Returns an array containing all entries of the priority queue.
   * Altering the returned array will not affect the queue, but mutating
   * the array's elements can cause unintended side effects.
   *
   * This function makes no guarantees on the ordering of the returned elements;
   * they will almost certainly be unsorted.
   */
  toArray(): Type[] {
    return this.heap.slice(0);
  }
}