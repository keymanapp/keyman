/**
 * A token
 * 
 * TODO: this should be an opaque type!
 */
type Token = number;

/**
 * The different message kinds.
 */
type MessageKind = 'initialize' | 'ready' | 'predict' | 'suggestions';

type Message = InitializeMessage | ReadyMessage;

interface InitializeMessage {
  message: 'initialize';
  /**
   * Path to the model. There are no concrete restrictions on the path
   * to the model, so long as the LMLayer can successfully use it to
   * initialize the model.
   */
  model: string;
  configuration: {
    /**
     * Whether the platform supports right contexts.
     * The absence of this rule implies false.
     */
    supportsRightContexts?: false,

    /**
     * Whether the platform supports deleting to the right.
     * The absence of this rule implies false.
     */
    supportsDeleteRight?: false,

    /**
     * The maximum amount of UTF-16 code units that the keyboard will
     * provide to the left of the cursor.
     */
    maxLeftContextCodeUnits: 32,

    /**
     * The maximum amount of code units that the keyboard will provide to
     * the right of the cursor. The absence of this rule implies 0.
     * See also, [[supportsRightContexts]].
     */
    maxRightContextCodeUnits: 32,
  }
}

interface ReadyMessage {
  message: 'ready';
  configuration: {
    /**
     * How many UTF-16 code units maximum to send as the context to the
     * left of the cursor ("left" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * TODO: Will this ever bisect graphical cluster boundaries?
     */
    leftContextCodeUnits: number,

    /**
     * How many UTF-16 code units maximum to send as the context to the
     * right of the cursor ("right" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * TODO: Will this ever bisect graphical cluster boundaries?
     */
    rightContextCodeUnits: number,
  };
}

interface PredictMessage {
  message: 'predict';
  token: Token;
  context: Context;
  transform: Transform;
} 