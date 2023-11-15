/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMCoreActionHandler.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-21.
 * 
 */

#import <Foundation/Foundation.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMActionOperation : NSObject
-(instancetype)initForSimpleAction:(CoreAction*)action;
-(instancetype)initForCompositeAction:(NSString*)textToInsert backspaceCount:(int)backspaces;
-(BOOL)hasTextToInsert;
-(BOOL)hasBackspaces;
-(BOOL)isBackspaceOnlyScenario;
-(BOOL)isTextOnlyScenario;
-(BOOL)isTextAndBackspaceScenario;
@property (readonly) CoreAction *action;
@property (readonly) NSString *textToInsert;
@property (readonly) int backspaceCount;
@property (readonly) BOOL isForSimpleAction;
@property (readonly) BOOL isForCompositeAction;
@end

@interface KMActionHandlerResult : NSObject
@property (readonly) BOOL handledEvent;
@property (readonly) NSArray *operations;
@property (readonly) NSString *textToInsert;
@property (readonly) int backspaceCount;

-(instancetype)initForActions:(NSArray*)actions handledEvent:(BOOL)handledEvent backspaceCount:(int) backspaces textToInsert:(NSString*) text;
@end

@interface KMCoreActionHandler : NSObject
-(instancetype)initWithActions:(NSArray*)actions keyCode: (unsigned short)keyCode;
-(KMActionHandlerResult*)handleActions;
@end

NS_ASSUME_NONNULL_END
