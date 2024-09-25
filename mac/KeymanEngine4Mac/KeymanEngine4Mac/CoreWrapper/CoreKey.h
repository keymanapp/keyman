/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-20.
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface CoreKey : NSObject
@property (nonatomic, readonly) NSUInteger keyCode;
@property (nonatomic, readonly) NSUInteger keyModifiers;
-(instancetype)init:(NSUInteger)code modifiers:(NSUInteger)modifiers;
-(BOOL)hasLeftAlt;
-(BOOL)hasRightAlt;
-(BOOL)hasAlt;
@end

NS_ASSUME_NONNULL_END
