/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CachedContext.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-11.
 * 
 */

#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMContext : NSObject

@property (readonly) NSString *currentContext;

-(instancetype)init NS_DESIGNATED_INITIALIZER;
-(instancetype)initWithString:(NSString*)initialContext;
-(void)resetContext:(NSString*)newContext;
-(void)addSubtring:(NSString*)string;
-(void)replaceSubstring:(NSString*)newText count:(int)count;
-(void)deleteLastCodePoint;
-(void)applyAction:(CoreAction*)action keyDownEvent:(nonnull NSEvent *)event;
-(BOOL)isInvalid;
-(void)invalidateContext;

@end

NS_ASSUME_NONNULL_END
