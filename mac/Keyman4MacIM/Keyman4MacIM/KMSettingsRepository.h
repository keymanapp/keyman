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
- (BOOL)keyman18DataMigrationNeeded;
- (BOOL)keyman19SettingsMigrationNeeded;
- (void)migrateSettingsForKeyman18;
- (void)setDataModelVersionIfNecessary;
- (void)migrateSettingsForKeyman19;
- (BOOL)migrateInputMethodSettingsToAppGroup;
- (NSString *)readSelectedKeyboard;
- (void)writeSelectedKeyboard:(NSString *)selectedKeyboard;
- (NSArray *)readActiveKeyboards;
- (void)writeActiveKeyboards: (NSArray *) keyboards;
- (void)clearActiveKeyboards;
- (NSDictionary *)readOptionsForSelectedKeyboard;
- (void)writeOptionForSelectedKeyboard:(NSString *)key withValue:(NSString*)value;
- (BOOL)readShowOskOnActivate;
- (void)writeShowOskOnActivate:(BOOL)show;
- (BOOL)readForceSentryError;
@end

NS_ASSUME_NONNULL_END
