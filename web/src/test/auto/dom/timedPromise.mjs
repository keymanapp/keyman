
/**
 * Acts as a Promise-form of `setTimeout`.
 * @param {*} func A function to run after the specified amount of time.
 * @param {*} time The timeout to wait.
 * @returns {*} A `Promise` that will either resolve or reject after the specified amount of time.
 */
export default function timedPromise(time) {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      try {
        resolve();
      } catch (err) {
        reject(err);
      }
    }, time);
  });
}