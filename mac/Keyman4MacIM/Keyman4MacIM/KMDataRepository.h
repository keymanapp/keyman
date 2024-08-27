/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2024-07-30.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMDataRepository : NSObject
// keymanDataDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
@property (readonly) NSURL *keymanDataDirectory;

// keymanKeyboardsDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
@property (readonly) NSURL *keymanKeyboardsDirectory;

+ (KMDataRepository *)shared;
- (void)createDataDirectoryIfNecessary;
- (void)createKeyboardsDirectoryIfNecessary;
- (BOOL)migrateData;
@end

NS_ASSUME_NONNULL_END
