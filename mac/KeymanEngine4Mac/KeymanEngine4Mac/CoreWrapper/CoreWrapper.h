/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2022-12-12.
 */

#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#import "CoreHelper.h"
#import "CoreAction.h"
#import "CoreKeyOutput.h"
#import "CoreKey.h"

NS_ASSUME_NONNULL_BEGIN

@interface CoreWrapper : NSObject

@property (strong, nonatomic, readonly) NSString *keyboardId;
@property (strong, nonatomic, readonly) NSString *keyboardVersion;

-(instancetype)initWithHelper:(CoreHelper*)helper kmxFilePath:(nullable NSString*)path;
-(BOOL)setOptionsForCore: (NSString *) key value:(NSString *) value;
-(void)changeKeyboardWithKmxFilePath:(NSString*)path;
-(NSArray*)getKeyList;
-(CoreKeyOutput*)processEvent:(nonnull NSEvent *)event;
-(CoreKeyOutput*)processMacVirtualKey:(unsigned short)macKeyCode
              withModifiers:(NSEventModifierFlags)modifierState
                withKeyDown:(BOOL)isKeyDown;
-(void)setContextIfNeeded:(NSString*)context;
-(NSString*)contextDebug;
-(void)clearCoreContext;
-(void)dealloc;

@end

NS_ASSUME_NONNULL_END
