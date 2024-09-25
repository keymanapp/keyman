/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-25.
 * 
 */

#import <Foundation/Foundation.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMModifierMapping : NSObject
-(instancetype)init:(CoreKeyboardInfo*)keyboardInfo;
-(BOOL)optionKeysUnused;
-(BOOL)bothOptionKeysGenerateRightAlt;
@end

NS_ASSUME_NONNULL_END
