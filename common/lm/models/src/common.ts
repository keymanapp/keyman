namespace models {
  export function applyTransform(transform: Transform, context: Context): Context {
    // First, get the current context
    let fullLeftContext = context.left || '';
    let lLen = fullLeftContext.length;
    let lDel = lLen < transform.deleteLeft ? lLen : transform.deleteLeft;

    let leftContext = fullLeftContext.substring(0, lLen - lDel) + (transform.insert || '');

    let fullRightContext = context.right || '';
    let rLen = fullRightContext.length;
    let rDel = rLen < transform.deleteRight ? rLen : transform.deleteRight;

    let rightContext = fullRightContext.substring(rDel);

    return {
      left: leftContext,
      right: rightContext,
      startOfBuffer: context.startOfBuffer,
      endOfBuffer: context.endOfBuffer
    };
  }

  /**
   *
   * @param transform Merges one transform into another, mutating the first parameter to
   *                  include the effects of the second.
   * @param prefix
   */
  export function prependTransform(transform: Transform, prefix: Transform) {
    transform.insert = prefix.insert + transform.insert;
    transform.deleteLeft += prefix.deleteLeft;
    if(prefix.deleteRight) {
      transform.deleteRight = (transform.deleteRight || 0) + prefix.deleteRight;
    }
  }
}
