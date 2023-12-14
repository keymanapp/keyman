/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreAction.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-02-21.
 */

#import <Foundation/Foundation.h>
#import "CoreHelper.h"

NS_ASSUME_NONNULL_BEGIN
typedef enum {EndAction,
  CharacterAction,
  MarkerAction,
  AlertAction,
  CharacterBackspaceAction,
  MarkerBackspaceAction,
  PersistOptionAction,
  EmitKeystrokeAction,
  InvalidateContextAction,
  CapsLockAction,
} ActionType;

@interface CoreAction : NSObject
@property (readonly) ActionType actionType;
@property (strong, nonatomic, readonly) NSString *typeName;
@property (strong, nonatomic, readonly) NSString *content;
@property (readonly) int backspaceCount;
@property (strong, nonatomic, readonly) NSString *key;
@property (strong, nonatomic, readonly) NSString *value;
@property (readonly) int scope;
-(instancetype)initWithType: (ActionType)type actionContent:(NSString*)content backspaceCount:(int)backspaceCount key:(NSString*)key value:(NSString*)value scope:(UInt8)scope NS_DESIGNATED_INITIALIZER;
//must implement the designated initializer of the superclass
-(instancetype)init;
-(instancetype)initCharacterAction:(NSString*)content;
-(instancetype)initCharacterBackspaceAction:(NSString*)content;
-(instancetype)initMarkerBackspaceAction:(int)count;
-(instancetype)initPersistOptionAction:(NSString*)key value:(NSString*)value scope:(UInt8)scope;
-(BOOL)isCharacter;
-(BOOL)isCharacterBackspace;
-(BOOL)isMarker;
-(BOOL)isMarkerBackspace;

@end

NS_ASSUME_NONNULL_END
