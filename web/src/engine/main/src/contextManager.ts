import EventEmitter from 'eventemitter3';
import { type Keyboard, type KeyboardInterface, type OutputTarget } from '@keymanapp/keyboard-processor';

interface EventMap {
  'changedcontext': (target: OutputTarget, keyboard: Keyboard) => void;
}

export default abstract class ContextManager extends EventEmitter<EventMap> {
  abstract initialize(): void;

  abstract get activeTarget(): OutputTarget;
  // activeKeyboard: Keyboard; // probably belongs here.

  insertText(kbdInterface: KeyboardInterface, Ptext: string, PdeadKey: number) {
    // Find the correct output target to manipulate.
    const outputTarget = this.activeTarget;

    if(outputTarget != null) {
      if(Ptext != null) {
        kbdInterface.output(0, outputTarget, Ptext);
      }

      if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
        kbdInterface.deadkeyOutput(0, outputTarget, PdeadKey);
      }

      outputTarget.invalidateSelection();
      return true;
    }
    return false;
  }
}

// Intended design:
// - SiteContextManager - for website, document-aware context management
//   - app/embed
// - PassthroughContextManager - for WebView-hosted, app-embedded context management.
//   - app/web