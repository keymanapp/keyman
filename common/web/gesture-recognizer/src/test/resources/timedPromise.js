/**
 * Acts as a Promise-form of `setTimeout`.
 * @param {*} func A function to run after the specified amount of time.
 * @param {*} time The timeout to wait.
 * @returns {*} A `Promise` that will either resolve or reject after the specified amount of time.
 */
 const timedPromise = (func, time) => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      try {
        func();
        resolve();
      } catch (err) {
        reject(err);
      }
    }, time);
  });
}

(function () {
  // Lets the function be available both in the browser and in Node.
  if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = timedPromise;
  }
}());