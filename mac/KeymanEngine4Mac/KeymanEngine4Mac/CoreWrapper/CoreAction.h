/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreAction.h
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2023-02-21.
 * 
 * Description...
 */

#import <Foundation/Foundation.h>
#import "keyboardprocessor.h"
#import "CoreHelper.h"

NS_ASSUME_NONNULL_BEGIN
typedef enum {EndAction,
  CharacterAction,
  MarkerAction,
  AlertAction,
  BackspaceAction,
  PersistOptionAction,
  EmitKeystrokeAction,
  InvalidateContextAction,
  CapsLockAction
} ActionType;

@interface CoreAction : NSObject
@property (readonly) ActionType actionType;
@property (strong, nonatomic, readonly) NSString *typeName;
@property (strong, nonatomic, readonly) NSString *content;
@property (readonly) int backspaceCount;
-(instancetype)initWithType: (ActionType)type actionContent:(NSString*)content backspaceCount:(int)backspaceCount NS_DESIGNATED_INITIALIZER;
//must implement the designated initializer of the superclass
-(instancetype)init;
-(instancetype)initWithActionStruct:(km_kbp_action_item*)actionStruct coreHelper:(CoreHelper*)helper;
-(instancetype)initCharacterAction:(NSString*)content;
-(instancetype)initBackspaceAction:(int)count;
-(NSDictionary*) legacyDictionaryActionForActionObject:(CoreAction*)action;
@end

NS_ASSUME_NONNULL_END
