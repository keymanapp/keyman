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

// for classifying the state of the settings (UserDefaults) and data
// used for determining whether migration is needed of the UserDefaults and Keyman packages
// to a new format and/or location
typedef NS_ENUM(NSInteger, SettingsState) {
  KeymanSettingsNotFound = 0,
  KeymanSettingsVersion17 = 17,
  KeymanSettingsVersion18 = 18,
  KeymanSettingsVersionCurrent = 19
};

@interface KMSettingsRepository : NSObject
+ (KMSettingsRepository *)shared;
- (instancetype)init;
- (SettingsState)determineSettingsState;
- (void)createSharedSettingsIfNecessary; // introduced with Keyman 19
- (void)migrateSettingsFromKeyman17;
- (void)migrateSettingsFromKeyman18;
- (NSString *)readSelectedKeyboard;
- (void)writeSelectedKeyboard:(NSString *)selectedKeyboard;
- (NSArray *)readEnabledKeyboards;
- (void)writeEnabledKeyboards: (NSArray *) keyboards;
- (void)clearEnabledKeyboards;
- (NSDictionary *)readOptionsForSelectedKeyboard;
- (void)writeOptionForSelectedKeyboard:(NSString *)key withValue:(NSString*)value;
- (BOOL)readShowOskOnActivate;
- (void)writeShowOskOnActivate:(BOOL)show;
- (BOOL)readForceSentryError;
@end

NS_ASSUME_NONNULL_END
