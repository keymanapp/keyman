/*
 * Unit tests for the priority queue.
 */

var assert = require('chai').assert;
var PriorityQueue = require('../').models.PriorityQueue;

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
  })
});