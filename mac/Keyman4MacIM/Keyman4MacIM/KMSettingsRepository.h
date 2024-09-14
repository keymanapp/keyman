/*
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
- (void)setDataModelVersionIfNecessary;
- (NSString *)readSelectedKeyboard;
- (void)writeSelectedKeyboard:(NSString *)selectedKeyboard;
- (NSArray *)readActiveKeyboards;
- (void)writeActiveKeyboards: (NSArray *) keyboards;
- (void)clearActiveKeyboards;
- (NSDictionary *)readOptionsForSelectedKeyboard;
- (void)writeOptionForSelectedKeyboard:(NSString *)key withValue:(NSString*)value;
- (BOOL)readAlwaysShowOsk;
- (void)writeAlwaysShowOsk:(BOOL)alwaysShowOsk;
- (BOOL)readUseVerboseLogging;
- (void)writeUseVerboseLogging:(BOOL)verboseLogging;
@end

NS_ASSUME_NONNULL_END
