/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * ActionArrayOptimizer.m
 * Keyman
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

/*
 * This optimizes the array of CoreAction objects to best simplify
 * output by the Input Method based on its use of Keyman Core.
 */
-(NSArray*)optimize:(NSArray*)actionArray {
  
  NSMutableArray *optimizedArray = [[NSMutableArray alloc] init];
  
  /*
    loop through actions in order if we find an EndAction, do not save to
    optimizedArray if we find a CharacterBackspaceAction then remove the last action (as
    long as the type of backspace matches the last action) and do not save the
    BackspaceAction to optimizedArray otherwise, simply copy to optimizedArray
  */
  for (CoreAction *action in [actionArray objectEnumerator])
  {
    if ([self checkForUnnecessaryAction:action]) {
      continue;
    } else if (optimizedArray.count > 0) {
      CoreAction *lastAction = optimizedArray.lastObject;
      if ((action.isCharacterBackspace) && (lastAction.isCharacter)) {
          [optimizedArray removeLastObject];
          continue;
        } else if ((action.isMarkerBackspace) && (lastAction.isMarker)) {
          [optimizedArray removeLastObject];
          continue;
        }
    }
    
    [optimizedArray insertObject:action atIndex:optimizedArray.count];
  }

  return optimizedArray;
}

/* returns YES if the actions can either be combined or eliminate each other */
-(BOOL)canCombineNextAction:(CoreAction*)nextAction withCurrentAction:(CoreAction*)currentAction {
  BOOL canCombine = NO;
  // actions are same type
  if (nextAction.actionType == currentAction.actionType) {
    // can be combined if they are backspaces or characters
    canCombine = ((nextAction.isCharacter) || (nextAction.isCharacterBackspace));
  } else {
    // if not same type, can be combined if deleting a character
    canCombine = [self canEliminateActions:nextAction withCurrentAction:currentAction];
  }
  return canCombine;
}

/* returns YES if the actions eliminate each other */
-(BOOL)canEliminateActions:(CoreAction*)nextAction withCurrentAction:(CoreAction*)currentAction {
  return ((currentAction.isCharacter) && (nextAction.isCharacterBackspace));
}

/* returns YES if the action is unnecessary and should be removed */
-(BOOL)checkForUnnecessaryAction:(CoreAction*)action {
  return action.actionType == EndAction;
}

/*
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
    // if not same type, can be combined if deleting a character
    if ((currentAction.actionType == CharacterAction) && (nextAction.actionType == BackspaceAction)) {
      combinedAction = [self combineBackspaceAction:nextAction withCharacterAction:currentAction];
    }
  }
  return combinedAction;
}
*/
/*
-(CoreAction*)eliminateActions:(CoreAction*)nextAction withCurrentAction:(CoreAction*)currentAction {
  CoreAction* combinedAction = nil;
  if ((currentAction.actionType == CharacterAction) && (nextAction.actionType == BackspaceAction)) {
    combinedAction = [self combineBackspaceAction:nextAction withCharacterAction:currentAction];
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
 */

@end
