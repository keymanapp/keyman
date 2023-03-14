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
enum ActionType {EndAction,
  CharacterAction,
  MarkerAction,
  AlertAction,
  BackspaceAction,
  PersistOptionAction,
  EmitKeystrokeAction,
  InvalidateContextAction,
  CapsLockAction
};

@interface CoreAction : NSObject
@property (readonly) enum ActionType actionType;
@property (strong, nonatomic, readonly) NSString *typeName;
@property (strong, nonatomic, readonly) NSString *content;
@property (readonly) int backspaceCount;
-(instancetype)initWithActionStruct:(km_kbp_action_item*)actionStruct coreHelper:(CoreHelper*)helper;
-(instancetype)initWithString:(NSString*)content;
-(instancetype)initWithBackspaceCount:(int)count;
-(NSDictionary*) legacyDictionaryActionForActionObject:(CoreAction*)action;
@end

NS_ASSUME_NONNULL_END
