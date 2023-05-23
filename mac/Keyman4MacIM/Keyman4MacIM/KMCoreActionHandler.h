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
#import "KMContext.h"

NS_ASSUME_NONNULL_BEGIN

@interface KMActionOperation : NSObject
-(instancetype)initForSimpleAction:(CoreAction*)action;
-(instancetype)initForCompositeAction:(NSString*)textToInsert backspaceCount:(int)backspaces useEvents:(BOOL)useEvents;
@property (readonly) CoreAction *action;
@property (readonly) NSString *textToInsert;
@property (readonly) int backspaceCount;
@property (readonly) BOOL useEvents;
@property (readonly) BOOL isForSimpleAction;
@property (readonly) BOOL isForCompositeAction;
@end

@interface KMActionHandlerResult : NSObject
@property (readonly) BOOL handledEvent;
@property (readonly) NSArray *operations;
@property (readonly) NSString *textToInsert;
@property (readonly) int backspaceCount;
@property (readonly) BOOL useEvents;

-(instancetype)initForActions:(NSArray*)actions handledEvent:(BOOL)handledEvent backspaceCount:(int) backspaces textToInsert:(NSString*) text useEvents:(BOOL)useEvents;
@end

@interface KMCoreActionHandler : NSObject
-(instancetype)initWithActions:(NSArray*)actions context: (KMContext *)context event: (NSEvent *)event client:(id) client;
-(KMActionHandlerResult*)handleActions;
@end

NS_ASSUME_NONNULL_END
