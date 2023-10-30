/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreHelper.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-02-14.
 */

#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern UInt32 VirtualKeyMap[0x80];

@interface CoreHelper : NSObject

@property (assign, nonatomic) BOOL debugMode;

-(unichar const *) createUnicharStringFromNSString:(NSString *)string;
-(NSString *) createNSStringFromUnicharString:(unichar const *)string;
-(unsigned long long) unicharStringLength:(unichar const *)string;

-(instancetype)initWithDebugMode:(BOOL)debugMode;
-(void)logDebugMessage:(NSString *)format, ...;
-(unsigned short) macVirtualKeyToWindowsVirtualKey:(unsigned short) keyCode;
-(UInt32)macToKeymanModifier:(NSEventModifierFlags)modifiers;
-(NSArray*)optimizeActionArray:(NSArray*)actionArray;
-(NSString*)utf32ValueToString:(UInt32)scalarValue;
@end

NS_ASSUME_NONNULL_END
