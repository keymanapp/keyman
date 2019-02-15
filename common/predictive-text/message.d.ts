/*
 * Copyright (c) 2018 National Research Council Canada (author: Eddie A. Santos)
 * Copyright (c) 2018 SIL International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


/**	
 * A JavaScript string with the restriction that it must only	
 * contain Unicode scalar values.	
 *	
 * This means that any lone high surrogate must be paired with	
 * a low surrogate, if it exists. Lone surrogate code units are	
 * forbidden.	
 *	
 * See also: https://developer.mozilla.org/en-US/docs/Web/API/USVString	
 */	
type USVString = string;

/**
 * Tokens are signed 31-bit integers!
 */
type Token = number;

/**
 * The valid outgoing message kinds.
 */
type OutgoingMessageKind = 'ready' | 'suggestions';
type OutgoingMessage = ReadyMessage | SuggestionMessage;

/**
 * Tells the keyboard that the LMLayer is ready. Provides
 * negotiated configuration.
 */
interface ReadyMessage {
  message: 'ready';
  configuration: Configuration;
}

/**
 * Sends the keyboard an ordered list of suggestions.
 */
interface SuggestionMessage {
  message: 'suggestions';

  /**
   * Opaque, unique token that pairs this suggestions message
   * with the predict message that initiated it.
   */
  token: Token;

  /**
   * Ordered array of suggestions, most probable first, least
   * probable last.
   */
  suggestions: Suggestion[];
}

/**
 * Describes the capabilities of the keyboard's platform.
 * This includes upper bounds for how much text will be sent on each
 * prediction, as well as what operations the keyboard is allowed to do on the
 * underlying buffer.
 */
interface Capabilities {
  /**
   * The maximum amount of UTF-16 code units that the keyboard will provide to
   * the left of the cursor, as an integer.
   */
  maxLeftContextCodeUnits: number,

  /**
   * The maximum amount of code units that the keyboard will provide to the
   * right of the cursor, as an integer. The value 0 or the absence of this
   * rule implies that the right contexts are not supported.
   */
  maxRightContextCodeUnits?: number,

  /**
   * Whether the platform supports deleting to the right. The absence of this
   * rule implies false.
   */
  supportsDeleteRight?: false,
}

/**
 * Describes what kind of model to instantiate.
 */
type ModelDescription = DummyModelDescription | WordListModelDescription;
interface DummyModelDescription {
  type: 'dummy';
  futureSuggestions?: Suggestion[][];
}
interface WordListModelDescription {
  type: 'wordlist';
  /**
   * Words to predict, one word per entry in the array.
   */
  wordlist: string[];
}

/**
 * Configuration of the LMLayer, sent back to the keyboard.
 */
interface Configuration {
  /**
   * How many UTF-16 code units maximum to send as the context to the
   * left of the cursor ("left" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the left context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  leftContextCodeUnits: number;

  /**
   * How many UTF-16 code units maximum to send as the context to the
   * right of the cursor ("right" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the right context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  rightContextCodeUnits: number;
}

/**
 * Describes how to change a buffer at the cursor position.
 * first, you delete the specified amount amount from the left
 * and right, then you insert the provided text.
 */
interface Transform {
  /**
   * The Unicode scalar values (i.e., characters) to be inserted at the
   * cursor position.
   *
   * Corresponds to `s` in com.keyman.KeyboardInterface.output.
   */
  insert: USVString;

  /**
   * The number of code units to delete to the left of the cursor.
   *
   * Corresponds to `dn` in com.keyman.KeyboardInterface.output.
   */
  deleteLeft: number;

  /**
   * The number of code units to delete to the right of the cursor.
   * Not available on all platforms.
   */
  deleteRight?: number;
}

/**
 * The text and environment surrounding the insertion point (text cursor).
 */
interface Context {
  /**
   * Up to maxLeftContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the left of the insertion point in the
   * buffer. If there is nothing to the left of the buffer, this is
   * an empty string.
   */
  left: USVString;

  /**
   * Up to maxRightContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the right of the insertion point in the
   * buffer. If there is nothing to the right of the buffer, this is
   * an empty string.
   * 
   * This property may be missing entirely.
   */
  right?: USVString;

  /**
   * Whether the insertion point is at the start of the buffer.
   */
  startOfBuffer: boolean;

  /**
   * Whether the insertion point is at the end of the buffer.
   */
  endOfBuffer: boolean;
}

/**
 * A concrete suggestion
 */
interface Suggestion {
  /**
   * The suggested update to the buffer. Note that this transform should
   * be applied AFTER the instigating transform, if any.
   */
  transform: Transform;

  /**
   * A string to display the suggestion to the typist.
   * This should aid the typist understand what the transform
   * will do to their text.
   * 
   * When suggesting a word, `displayAs` should be that entire word.
   */
  displayAs: string;
}
