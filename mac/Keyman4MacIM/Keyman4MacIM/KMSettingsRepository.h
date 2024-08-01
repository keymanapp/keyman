/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMSettingsRepository.h
 * Keyman
 *
 * Created by Shawn Schantz on 2024-07-29.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMSettingsRepository : NSObject
+ (KMSettingsRepository *)shared;
- (BOOL)dataMigrationNeeded;
- (void)convertSettingsForMigration;
- (void)createStorageFlagIfNecessary;
@end

NS_ASSUME_NONNULL_END
