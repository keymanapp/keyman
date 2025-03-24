/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-23.
 * 
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface CoreKeyboardInfo : NSObject
@property (nonatomic, readonly) NSArray *keyArray;
@property (nonatomic, readonly) NSString *keyboardId;
@property (readonly) BOOL containsAlt;
@property (readonly) BOOL containsLeftAlt;
@property (readonly) BOOL containsRightAlt;
-(instancetype)init:(NSString*)id keyArray:(NSArray*) keys;
@end

NS_ASSUME_NONNULL_END
