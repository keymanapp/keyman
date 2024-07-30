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
+ (KMDataRepository *)shared;
- (void)migrateResources;
- (NSString *)keymanDataDirectory;
@end

NS_ASSUME_NONNULL_END
