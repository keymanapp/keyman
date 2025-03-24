/*
 * Unit tests for the priority queue.
 */

import { assert } from 'chai';
import { PriorityQueue } from '@keymanapp/web-utils';

describe('Priority queue', function() {
  it('can act as a min-heap', function () {
    let input = [1, 10, 2, 9, 3, 8, 4, 7, 5, 6];

    let queue = new PriorityQueue((a, b) => a - b);
    input.forEach((input) => queue.enqueue(input));

    assert.equal(queue.peek(), 1);
    assert.equal(queue.count, 10);

    for(let i = 1; i <= 10; i++) {
      assert.equal(queue.dequeue(), i);
    }

    assert.equal(queue.count, 0);
  });

  it('initializes well from pre-existing values', function() {
    let input = [1, 10, 2, 9, 3, 8, 4, 7, 5, 6];
    let originalInput = input.slice(0);
    let queue = new PriorityQueue((a, b) => a - b, input);

    assert.deepEqual(input, originalInput);

    assert.equal(queue.peek(), 1);
    assert.equal(queue.count, 10);

    for(let i = 1; i <= 10; i++) {
      assert.equal(queue.dequeue(), i);
    }

    assert.equal(queue.count, 0);
  });

  it('can act as a max-heap', function () {
    let input = [1, 10, 2, 9, 3, 8, 4, 7, 5, 6];

    let queue = new PriorityQueue((a, b) => b - a);
    input.forEach((input) => queue.enqueue(input));

    assert.equal(queue.peek(), 10);
    assert.equal(queue.count, 10);

    for(let i = 10; i >= 1; i--) {
      assert.equal(queue.dequeue(), i);
    }

    assert.equal(queue.count, 0);
  });

  it('operates correctly when used interactively', function() {
    // min-heap version.
    let queue = new PriorityQueue((a, b) => a - b);

    queue.enqueueAll([1, 10, 3, 8, 5]);
    assert.equal(queue.count, 5);

    assert.equal(queue.dequeue(), 1);
    assert.equal(queue.count, 4);

    queue.enqueue(2);
    assert.equal(queue.count, 5);

    assert.equal(queue.dequeue(), 2);
    assert.equal(queue.count, 4);

    queue.enqueue(7);
    queue.enqueue(6);
    assert.equal(queue.count, 6);

    assert.equal(queue.dequeue(), 3);
    assert.equal(queue.count, 5);

    queue.enqueue(13);
    queue.enqueue(0);
    assert.equal(queue.count, 7);

    assert.equal(queue.dequeue(), 0);
    assert.equal(queue.count, 6);

    queue.enqueue(1);
    queue.enqueue(1);
    assert.equal(queue.count, 8);

    assert.equal(queue.dequeue(), 1);
    assert.equal(queue.count, 7);

    assert.equal(queue.dequeue(), 1);
    assert.equal(queue.count, 6);
  });

  it('properly batch-enqueues', function() {
    let queue = new PriorityQueue((a, b) => a - b);

    queue.enqueueAll([1, 10, 3, 8, 5]);
    assert.equal(queue.count, 5);

    assert.equal(queue.dequeue(), 1);
    assert.equal(queue.count, 4);

    let batch2 = [2, 7, 6, 13, 0, 1, 1];
    queue.enqueueAll(batch2);

    let correctOrder = [0, 1, 1, 2, 3, 5, 6, 7, 8, 10, 13];
    while(correctOrder.length > 0) {
      assert.equal(queue.dequeue(), correctOrder.shift());
    }
  });
});