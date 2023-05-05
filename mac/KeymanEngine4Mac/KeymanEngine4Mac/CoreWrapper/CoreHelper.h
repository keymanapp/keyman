/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreHelper.h
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2023-02-14.
 */

#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface CoreHelper : NSObject
-(instancetype)init;
-(unsigned short) macVirtualKeyToWindowsVirtualKey:(unsigned short) keyCode;
-(UInt32)macToKeymanModifier:(NSEventModifierFlags)modifiers;
-(NSArray*)actionObjectArrayToLegacyActionMapArray:(NSArray*)actionArray;
-(NSArray*)optimizeActionArray:(NSArray*)actionArray;
-(NSString*)utf32ValueToString:(UInt32)scalarValue;
@end

NS_ASSUME_NONNULL_END
