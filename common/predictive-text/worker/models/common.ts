namespace models {
  export class Common {
    static applyTransform(transform: Transform, context: Context): Context {
      // First, get the current context
      let fullLeftContext = context.left || '';
      let lLen = fullLeftContext.length;
      let lDel = lLen < transform.deleteLeft ? lLen : transform.deleteLeft;

      let leftContext = fullLeftContext.substr(0, lLen - lDel) + (transform.insert || '');

      let fullRightContext = context.right || '';
      let rLen = fullRightContext.length;
      let rDel = rLen < transform.deleteRight ? rLen : transform.deleteRight;

      let rightContext = fullRightContext.substr(rDel);

      return {
        left: leftContext,
        right: rightContext,
        startOfBuffer: context.startOfBuffer,
        endOfBuffer: context.endOfBuffer
      };
    }
  }
}