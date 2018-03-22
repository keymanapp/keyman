package com.tavultesoft.kmea.util;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Many thanks to https://gist.github.com/KlausBrunner/4110226.
 *
 * Note that once our minimum Android API version is 24, we can use the far superior
 * java.util.concurrent.CompletableFuture class.
 *
 * https://developer.android.com/reference/java/util/concurrent/CompletableFuture.html
 */

public class SimpleFuture<T> implements Future<T> {
  private final CountDownLatch latch = new CountDownLatch(1);
  private T value;
  private int id;

  private static AtomicInteger seed = new AtomicInteger(0);

  public SimpleFuture() {
    // Avoids race conditions while ensuring each SimpleFuture gets its own unique ID.
    this.id = seed.getAndIncrement();
  }

  public SimpleFuture(T result) {
    this.id = seed.getAndIncrement();
    this.value = result;
  }

  public int getId() {
    return this.id;
  }

  @Override
  public boolean cancel(boolean mayInterruptIfRunning) {
    return false;
  }

  @Override
  public boolean isCancelled() {
    return false;
  }

  @Override
  public boolean isDone() {
    return latch.getCount() == 0;
  }

  @Override
  public T get() throws InterruptedException {
    latch.await();
    return value;
  }

  @Override
  public T get(long timeout, TimeUnit unit) throws InterruptedException, TimeoutException {
    if (latch.await(timeout, unit)) {
      return value;
    } else {
      throw new TimeoutException();
    }
  }

  // calling this more than once doesn't make sense, and won't work properly in this implementation. so: don't.
  //
  // This also really should be protected, but we need a lot of refactoring before that's viable for our current
  // use case.
  public void put(T result) {
    value = result;
    latch.countDown();
  }
}
