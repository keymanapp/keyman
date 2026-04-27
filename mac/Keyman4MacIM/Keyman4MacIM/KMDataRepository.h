/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2024-07-30.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern NSString *const kKeymanGroupId;

@interface KMDataRepository : NSObject

// keyman18DataDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
@property (readonly) NSURL *keyman18DataDirectory;

// keyman18KeyboardsDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
@property (readonly) NSURL *keyman18KeyboardsDirectory;

@property (readonly) NSURL *keyman19ContainerDirectory;
@property (readonly) NSURL *keyman19KeyboardsDirectory;

+ (KMDataRepository *)shared;
- (void)createKeyman18DataDirectoryIfNecessary;
- (void)createKeyboardsDirectoryIfNecessary;
- (void)createKeyman19SharedDirectoriesIfNecessary;
- (BOOL)migrateDataForKeyman18;
- (BOOL)migrateDataForKeyman19;
- (NSString*)buildFullPath:(NSString *)fromPartialPath;
- (NSString*)trimToPartialPath:(NSString *)fromFullPath;
- (NSString *)buildPartialPathFrom:(NSString *)keyboardSubdirectory keyboardFile:(NSString *)kmxFilename;
@end

NS_ASSUME_NONNULL_END
