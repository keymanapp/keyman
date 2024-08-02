/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMDataRepository.h
 * Keyman
 *
 * Created by Shawn Schantz on 2024-07-30.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMDataRepository : NSObject
@property (readonly) NSURL *keymanDataDirectory;      // '~/Library/Application Support/com.keyman.app'
@property (readonly) NSURL *keymanKeyboardsDirectory;
// keymanKeyboardsDirectory = '~/Library/Application Support/com.keyman.app/Keyman-Keyboards'
+ (KMDataRepository *)shared;
- (void)createDataDirectoryIfNecessary;
- (void)createKeyboardsDirectoryIfNecessary;
- (BOOL)migrateData;
@end

NS_ASSUME_NONNULL_END
