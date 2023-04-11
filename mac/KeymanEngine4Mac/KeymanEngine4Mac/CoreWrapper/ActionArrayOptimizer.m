/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * ActionArrayOptimizer.m
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2023-02-23.
 * 
 * Manipulates an array of CoreAction objects created in response to the processing
 * of a key and reduces them to the minimal representation. For example, a character
 * that causes a re-ordering of the context may cause the character to be deleted.
 * Instead of emitting it twice and deleting it once, just emit it once.
 */

#import "ActionArrayOptimizer.h"

@implementation ActionArrayOptimizer

-(NSArray*)actionArrayToOptimizedActionArray:(NSArray*)actionArray {
  
  NSMutableArray *optimizedArray = [[NSMutableArray alloc] init];
  CoreAction *nextAction = nil;
  
  // loop through actions in reverse order
  for (CoreAction *action in [actionArray reverseObjectEnumerator])
  {
    if (nextAction) {
      // check whether we should skip this action as unnecessary
      if ([self checkForUnnecessaryAction:nextAction]) {
        nextAction = action;
      } else if ([self canCombineNextAction:nextAction withCurrentAction:action]) {
        // check whether we can combine the current action with nextAction

        // if yes, then combine them
        CoreAction *combined = [self combineNextAction:nextAction withCurrentAction:action];
        
        // combineActions can create a combined action
        if (combined) {
          nextAction = combined;
        } else {
        // or it can create nil because they eliminated each other
          nextAction = nil;
        }
      } else {
        // cannot combine, so insert nextAction at beginning since we are iterating in reverse
        [optimizedArray insertObject:nextAction atIndex:0];
        // set new nextAction to current action
        nextAction = action;
      }
    } else {
      // need to set the followingAction because either
      // 1. this is the first time through loop, or
      // 2. combining the previous actions caused them to cancel each other out
      nextAction = action;
    }
  }

  if (nextAction) {
    // we are iterating in reverse, so insert at beginning of the array
    [optimizedArray insertObject:nextAction atIndex:0];
  }

  return optimizedArray;
}

/* returns YES if the actions can either be combined or eliminate each other */
-(BOOL)canCombineNextAction:(CoreAction*)nextAction withCurrentAction:(CoreAction*)currentAction {
  BOOL canCombine = NO;
  // actions are same type
  if (nextAction.actionType == currentAction.actionType) {
    // can be combined if they are backspaces or characters
    canCombine = ((nextAction.actionType == CharacterAction) || (nextAction.actionType == BackspaceAction));
  } else {
    // if not same type, can be combined if deleting a character
    canCombine = ((currentAction.actionType == CharacterAction) && (nextAction.actionType == BackspaceAction));
  }
  return canCombine;
}

/* returns YES if the action is unnecessary and should be removed */
-(BOOL)checkForUnnecessaryAction:(CoreAction*)action {
  return action.actionType == EndAction;
}

-(CoreAction*)combineNextAction:(CoreAction*)nextAction withCurrentAction:(CoreAction*)currentAction {
  CoreAction* combinedAction = nil;
  
  // actions are same type
  if (nextAction.actionType == currentAction.actionType) {
    // combine if they are backspaces or characters
    if (nextAction.actionType == CharacterAction) {
      combinedAction = [self combineNextCharacterAction:nextAction withCurrentCharacterAction:currentAction];
    } else if (nextAction.actionType == BackspaceAction) {
      combinedAction = [self combineBackspaceAction:nextAction withBackspaceAction:currentAction];
    }
  } else {
    // if not same time, can be combined if deleting a character
    if ((currentAction.actionType == CharacterAction) && (nextAction.actionType == BackspaceAction)) {
      combinedAction = [self combineBackspaceAction:nextAction withCharacterAction:currentAction];
    }
  }
  return combinedAction;
}

-(CoreAction*)combineNextCharacterAction:(CoreAction*)nextAction withCurrentCharacterAction:(CoreAction*)currentAction {
  NSString *combinedString = [currentAction.content stringByAppendingString:nextAction.content];
  return [[CoreAction alloc] initCharacterAction:combinedString];
}

-(CoreAction*)combineBackspaceAction:(CoreAction*)nextAction withBackspaceAction:(CoreAction*)currentAction {
  int count = currentAction.backspaceCount + nextAction.backspaceCount;
  return [[CoreAction alloc] initBackspaceAction:count];
}

-(CoreAction*)combineBackspaceAction:(CoreAction*)nextAction withCharacterAction:(CoreAction*)currentAction {
  
  return nil;
}

@end
