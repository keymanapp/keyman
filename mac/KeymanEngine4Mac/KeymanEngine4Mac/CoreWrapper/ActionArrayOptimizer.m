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
  
  /**
    loop through actions in order
    if we find an EndAction, do not save to optimizedArray
    if we find a CharacterBackspaceAction then remove the last action (as
    long as the type of backspace matches the type of the last action) and
    do not save the BackspaceAction to optimizedArray
    otherwise, simply copy the action to the optimizedArray
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

/* returns YES if the action is unnecessary and should be removed */
-(BOOL)checkForUnnecessaryAction:(CoreAction*)action {
  return action.actionType == EndAction;
}

@end
