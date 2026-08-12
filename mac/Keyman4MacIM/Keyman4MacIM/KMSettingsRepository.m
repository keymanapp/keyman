/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2024-07-29.
 *
 * Singleton object for reading and writing Keyman application settings.
 * Serves as an abstraction to StandardUserDefaults which is currently used to store application settings.
 */

#import "KMSettingsRepository.h"
#import "KMLogs.h"
#import "KMDataRepository.h"

/**
 * The UserDefaults key `KMActiveKeyboardsKey` identifies the list of installed keyboard which
 * the user has enabled to be included in the Keyman keyboard menu. This key name is a little confusing
 * because the only truly active keyboard is the one that Keyman is applying while typing.
 * So, with the migration of data for Keyman version 19, this key is renamed to `KMEnabledKeyboardsKey`
 * Every enabled keyboard will appear in the Keyman keyboards menu.
 * Only one of these keyboards can be selected at a time, and the selected keyboard is the one that
 * is actively being applied by Keyman with each keystroke.
 */
// the list of keyboards displayed in the Keyman keyboards menu, Keyman 18 and earlier
NSString *const kActiveKeyboardsKey = @"KMActiveKeyboardsKey";
// the list of keyboards displayed in the Keyman keyboards menu, as of Keyman 19
NSString *const kEnabledKeyboardsKey = @"KMEnabledKeyboardsKey";
// the single keyboard Keyman is currently using to process each keydown event
NSString *const kSelectedKeyboardKey = @"KMSelectedKeyboardKey";
// the maps of options saved for each keyboard
NSString *const kPersistedOptionsKey = @"KMPersistedOptionsKey";
// whether the OSK should be displayed when Keyman is activated
NSString *const kShowOskOnActivate = @"KMShowOskOnActivate";
// internal flag used for testing Sentry
NSString *const kForceSentryError = @"KMForceSentryError";

/**
 * The following constant "KMSavedStoresKey" is left here for documentation
 * though we have abandoned stores written to UserDefaults with this key because
 * they used a less-reliable numeric key prior to integration with Keyman Core.
 * It is replaced by the renamed "KMPersistedOptionsKey" which directly
 * represents what it is saving.
 */
NSString *const kKMDeprecatedPersistedOptionsKey = @"KMSavedStoresKey";
/**
 * The following constant "KMAlwaysShowOSKKey" is left here for documentation
 * but the related UI has been removed according to issue #12342
 */
NSString *const kAlwaysShowOSKKey = @"KMAlwaysShowOSKKey";
/**
 * The following constant "KMUseVerboseLogging" is left here for documentation
 * but it is obsolete and removed issue #11525
 */
NSString *const kUseVerboseLogging = @"KMUseVerboseLogging";

NSString *const kObsoletePathComponent = @"/Documents/Keyman-Keyboards";

/**
 * Store the version number of the data model in the UserDefaults with this key.
 * The first version, 1, is defined to indicate that we are storing the data/keyboards in the Library
 * directory instead of in the Documents directory.
 */
NSString *const kDataModelVersion = @"KMDataModelVersion";
NSInteger const kVersionStoreDataInLibraryDirectory = 1; // introduced with Keyman 18, obsolete in Keyman 19
NSInteger const kVersionStoreDataInGroupContainer = 2; // introduced with Keyman 19
NSInteger const kCurrentDataModelVersionNumber = kVersionStoreDataInGroupContainer;

@interface KMSettingsRepository ()
@property (nonatomic, strong) NSUserDefaults *appDefaults;
@property (nonatomic, strong) NSUserDefaults *groupDefaults;
@end

@implementation KMSettingsRepository

+ (KMSettingsRepository *)shared
{
  static KMSettingsRepository *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMSettingsRepository alloc] init];
  });
  return shared;
}

-(instancetype)init  {
  self = [super init];
  self.appDefaults = [NSUserDefaults standardUserDefaults];
  self.groupDefaults = [[NSUserDefaults alloc] initWithSuiteName:kKeymanGroupId];
  return self;
}

/**
 * Determines the current state of the Keyman settings (UserDefaults), indicating
 * whether migration is needed to a new format and/or location or whether this is
 * a first-time install and settings must be created.
 */
- (SettingsState)determineSettingsState {
  SettingsState state = KeymanSettingsVersionCurrent;
  
  // settings were moved to share app group beginning in Keyman 19
  if ([self settingsExistForAppGroup]) {
    os_log([KMLogs dataLog], "keyman shared settings exist, version is current");
    state = KeymanSettingsVersionCurrent;
  } else if ([self settingsExistForInputMethod]) {
    // In Keyman 18, KMDataModelVersion was added to settings and set to value of 1
    // No need to check the value, if it exists in the app UserDefaults, then it needs to be migrated
    if ([self version18SettingsExistForInputMethod]) {
      os_log([KMLogs dataLog], "keyman app (unshared) settings version indicates Keyman 18, packages stored in ~/Library");
      state = KeymanSettingsVersion18;
    } else {
      // no KMDataModelVersion key is found in the app UserDefaults
      os_log([KMLogs dataLog], "lack of keyman settings version indicates Keyman 17 or earlier, packages stored in ~/Documents");
      state = KeymanSettingsVersion17;
    }
  } else {
    // settings do not exist, must be a new install
    os_log([KMLogs dataLog], "keyman settings do not exist, must be created for new install");
    state = KeymanSettingsNotFound;
  }
  
  return state;
}

- (void)createSharedSettingsIfNecessary {
  // set kDataModelVersion for the current format
  [self writeCurrentDataModelVersion];
}

- (void)writeCurrentDataModelVersion {
  [self.groupDefaults setInteger:kCurrentDataModelVersionNumber forKey:kDataModelVersion];
}

/**
 * The dataModelVersion field will always exist for Keyman 18 and later, and starting
 * with Keyman 19, it will be located in the UserDefaults for the app group.
 */
- (BOOL)settingsExistForAppGroup
{
  return ([self.groupDefaults objectForKey:kDataModelVersion] != nil);
}

/**
 * If the selectedKeyboard exists in the app UserDefaults, as opposed to the shared app group
 * UserDefaults, then this is version 18 or earlier of Keyman.
 * For versions of Keyman, 17 and earlier, there was no`KMDataModelVersion` to check, but
 * the format and location of the settings was the same for those versions.
 */
- (BOOL)settingsExistForInputMethod
{
  return ([self.appDefaults objectForKey:kSelectedKeyboardKey] != nil);
}

/**
 * The dataModelVersion field will always exist for Keyman 18 and later, but, for version 18 only, it will be located
 * in the app UserDefaults instead of the shared app group UserDefaults
 */
- (BOOL)version18SettingsExistForInputMethod
{
  return ([self.appDefaults objectForKey:kDataModelVersion] != nil);
}

- (void)writeOptionForSelectedKeyboard:(NSString *)key withValue:(NSString*)value {
  NSDictionary *optionsMap = [self readOptionsForSelectedKeyboard];
  NSDictionary *newOptionsMap = nil;

  // if we can read an existing options map, then add the specified key-value pair
  if (optionsMap != nil) {
    NSMutableDictionary *mutableOptionsMap = [optionsMap mutableCopy];
    [mutableOptionsMap setObject:value forKey:key];
    newOptionsMap = mutableOptionsMap;
  } else {
    // if no options map exists, create a new one add the specified key-value pair
    newOptionsMap = [[NSDictionary alloc] initWithObjectsAndKeys:value, key, nil];
  }

  // write the options map for the selected keyboard to the dictionary of options
  NSString *selectedKeyboard = [self readSelectedKeyboard];
  os_log_info([KMLogs dataLog], "writeOptionForSelectedKeyboard, adding options map: %{public}@, to keyboard %{public}@", newOptionsMap, selectedKeyboard);
  [self writeKeyboardOptionsMap: selectedKeyboard withOptions:newOptionsMap];
}

- (void)writeKeyboardOptionsMap:(NSString *)keyboardName withOptions:(NSDictionary*) optionsMap {
  NSMutableDictionary *newFullOptionsMap = nil;
  os_log_debug([KMLogs dataLog], "writeKeyboardOptionsMap, adding options map: %{public}@, to keyboard %{public}@", optionsMap, keyboardName);

  NSDictionary *fullOptionsMap = [self readFullOptionsMap];
  // if we can read the existing full options map, then add for the specified keyboard
  if (fullOptionsMap != nil) {
    newFullOptionsMap = [fullOptionsMap mutableCopy];
    [newFullOptionsMap setObject:optionsMap forKey:keyboardName];
  } else {
    // otherwise, create the full options map and add for the specified keyboard
    newFullOptionsMap = [[NSMutableDictionary alloc] initWithObjectsAndKeys:optionsMap, keyboardName, nil];
  }

  [self writeFullOptionsMap:newFullOptionsMap];
}

- (NSString *)readSelectedKeyboard {
  return [self.groupDefaults objectForKey:kSelectedKeyboardKey];
}

- (void)writeSelectedKeyboard:(NSString *)selectedKeyboard {
  if (selectedKeyboard != nil) {
    [self.groupDefaults setObject:selectedKeyboard forKey:kSelectedKeyboardKey];
  }
}

- (NSMutableArray *)enabledKeyboards {
  NSMutableArray * enabledKeyboards = [[self.groupDefaults arrayForKey:kEnabledKeyboardsKey] mutableCopy];
  
  if (!enabledKeyboards) {
    enabledKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
  }
  return enabledKeyboards;
}

- (NSArray *)readEnabledKeyboards {
  os_log_debug([KMLogs dataLog], "KMSettingsRepository readEnabledKeyboards");
  NSArray *keyboardsArray = [self.groupDefaults arrayForKey:kEnabledKeyboardsKey];
  
  // if the kEnabledKeyboardsKey does not exist, then create an empty array
  if (!keyboardsArray) {
    os_log_debug([KMLogs dataLog], "kEnabledKeyboardsKey key not found in NSUserDefualts");
    keyboardsArray = [[NSArray alloc] init];
  }
  return keyboardsArray;
}

- (void)writeEnabledKeyboards: (NSArray *) keyboards {
  os_log_debug([KMLogs dataLog], "KMSettingsRepository writeEnabledKeyboards");
  [self.groupDefaults setObject:keyboards forKey:kEnabledKeyboardsKey];
}

- (void)clearEnabledKeyboards {
  os_log_debug([KMLogs dataLog], "KMSettingsRepository clearEnabledKeyboards");
  [self.groupDefaults setObject:nil forKey:kEnabledKeyboardsKey];
}

/**
 * returns dictionary of persisted options for the single selected keyboard
 */
- (NSDictionary *)readOptionsForSelectedKeyboard {
  NSDictionary *optionsMap = [self readFullOptionsMap];
  NSString *selectedKeyboard = [self readSelectedKeyboard];
  NSDictionary *selectedOptionsMap = [optionsMap objectForKey: selectedKeyboard];
  if (selectedOptionsMap == nil) {
    os_log_info([KMLogs dataLog], "no persisted options found in UserDefaults for keyboard %{public}@ ", selectedKeyboard);
  } else {
    for (NSString *key in selectedOptionsMap) {
      NSString *value = [selectedOptionsMap objectForKey:key];
      os_log_info([KMLogs dataLog], "option for keyboard %{public}@ key: %{public}@, value %{public}@", selectedKeyboard, key, value);
    }
  }
  return selectedOptionsMap;
}

/**
 * Read options map from the obsolete app defaults location: used only for migration to group defaults
 */
- (NSDictionary *)readFullOptionsMapFromAppDefaults {
  return [self.appDefaults dictionaryForKey:kPersistedOptionsKey];
}

/**
 * returns dictionary of all persisted options for all keyboards
 * (options are stored in UserDefaults as a map of maps)
 */
- (NSDictionary *)readFullOptionsMap {
  return [self.groupDefaults dictionaryForKey:kPersistedOptionsKey];
}

- (void)writeFullOptionsMap:(NSDictionary *) fullOptionsMap {
  [self.groupDefaults setObject:fullOptionsMap forKey:kPersistedOptionsKey];
}

- (void)removeAllOptions {
  return [self.groupDefaults removeObjectForKey:kPersistedOptionsKey];
}

- (BOOL)readShowOskOnActivate {
  return [self.groupDefaults boolForKey:kShowOskOnActivate];
}

- (void)writeShowOskOnActivate:(BOOL)show {
  [self.groupDefaults setBool:show forKey:kShowOskOnActivate];
}

- (BOOL)readForceSentryError {
  return [self.groupDefaults boolForKey:kForceSentryError];
}

// MARK: Settings Migration

- (void)migrateSettingsFromKeyman17 {
  os_log_debug([KMLogs dataLog], "migrating settings in UserDefaults from Keyman 17 ");
  
  [self migrateSettingsFromKeyman17ToAppGroup];

  // set kDataModelVersion for the current format
  [self writeCurrentDataModelVersion];

  [self removeMigratedInputMethodSettings];
}

/**
 * Used only for setting migration from Keyman 17 to 19
 * Read the settings in the input method's user defaults
 * Convert paths from full path to partial path
 * Write to share app group user defaults (and write to a different key for enabled keyboards)
 */
- (void)migrateSettingsFromKeyman17ToAppGroup {
  [self convertSelectedKeyboardPathFromKeyman17];
  [self convertActiveKeyboardArrayFromKeyman17];
  
  // read showOsk from one defaults suite to another
  if ([self.appDefaults objectForKey:kShowOskOnActivate] != nil) {
    BOOL showOsk = [self.appDefaults boolForKey:kShowOskOnActivate];
    [self.groupDefaults setBool:showOsk forKey:kShowOskOnActivate];
  }
  
  // read forceSentryError from one defaults suite to another
  if ([self.appDefaults objectForKey:kForceSentryError] != nil) {
    BOOL forceSentryError = [self.appDefaults boolForKey:kForceSentryError];
    [self.groupDefaults setBool:forceSentryError forKey:kForceSentryError];
  }
  
  [self convertOptionsPathsFromKeyman17];
}

/**
 * Convert the selectedKeyboard path from the full path of Keyman 17 to a partial path.
 * Read it from the app defaults and write it to the group defaults.
 */
- (void)convertSelectedKeyboardPathFromKeyman17 {
  NSString *selectedKeyboardPath = [self.appDefaults objectForKey:kSelectedKeyboardKey];
  if (selectedKeyboardPath != nil) {
    NSString *newPathString = [self trimObsoleteKeyboardPath:selectedKeyboardPath];
    
    if ([selectedKeyboardPath isNotEqualTo:newPathString]) {
      [self writeSelectedKeyboard:newPathString];
      os_log_debug([KMLogs dataLog], "converted selected keyboard setting from '%{public}@' to '%{public}@'", selectedKeyboardPath, newPathString);
    }
  }
}

/**
 * Convert the activeKeyboards array and the full path for each keyboard from Keyman 17 to
 * a partial path for each in the enabledKeyboards array.
 * Read the array from the app defaults and write it to the group defaults.
 */
- (void)convertActiveKeyboardArrayFromKeyman17 {
  // load from old location with old key name
  NSArray *activeKeyboards = [self.appDefaults arrayForKey:kActiveKeyboardsKey];
  if (activeKeyboards != nil) {
    NSMutableArray *enabledKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
    BOOL didConvert = NO;
    
    for (NSString *oldPath in activeKeyboards) {
      // shorten from full path to partial path
      NSString *newPath = [self trimObsoleteKeyboardPath:oldPath];
      if ([oldPath isNotEqualTo:newPath]) {
        [enabledKeyboards addObject:newPath];
        os_log_debug([KMLogs dataLog], "converted enabled keyboard from old path '%{public}@' to '%{public}@'", oldPath, newPath);
        // if we have adjusted at least one path, set flag
        didConvert = YES;
      } else {
        // if, somehow, the path does not need converting then retain it in new array
        [enabledKeyboards addObject:oldPath];
      }
    }
    
    // only write array to UserDefaults if we actually converted something
    if (didConvert) {
      [self writeEnabledKeyboards:enabledKeyboards];
    }
  }
}

/**
 * Convert the options paths map from the full path of Keyman 17 to a partial path.
 * Read it from the app defaults and write it to the group defaults.
 */
- (void)convertOptionsPathsFromKeyman17 {
  NSDictionary * optionsMap = [self readFullOptionsMapFromAppDefaults];
  NSMutableDictionary *mutableOptionsMap = nil;

  if (optionsMap != nil) {
    os_log_info([KMLogs configLog], "optionsMap != nil");
    mutableOptionsMap = [[NSMutableDictionary alloc] initWithCapacity:0];
    for (id key in optionsMap) {
      os_log_info([KMLogs configLog], "persisted options found in UserDefaults with key = %{public}@", key);
    }
    for (NSString *keyboardPath in optionsMap) {
      os_log_info([KMLogs configLog], "persisted options keybaord path = %{public}@", keyboardPath);
      NSDictionary *keyboardOptions = [optionsMap objectForKey:keyboardPath];
      
      NSString *newPathString = [self trimObsoleteKeyboardPath:keyboardPath];
      os_log_info([KMLogs configLog], "persisted options converted key = %{public}@", newPathString);

      if ([keyboardPath isNotEqualTo:newPathString]) {
        // insert options into new map with newly converted path as key
        [mutableOptionsMap setObject:keyboardOptions forKey:newPathString];
        os_log_debug([KMLogs dataLog], "converted option key from '%{public}@' to '%{public}@'", keyboardPath, newPathString);
      } else {
        // retain options that did not need converting
        [mutableOptionsMap setObject:keyboardOptions forKey:keyboardPath];
        os_log_debug([KMLogs dataLog], "no conversion needed, adding options for '%{public}@'", keyboardPath);
      }
    }
    // write the full option map for all keyboards to the group UserDefaults
    [self writeFullOptionsMap:mutableOptionsMap];
  }
}

/**
 * To convert the keyboard path for the new location, just trim the parent directory from the path
 * No need to repeatedly store the parent directory with the path of each keyboard
 * If the old directory is not found in the string, then return the string unchanged
 */
- (NSString *)trimObsoleteKeyboardPath:(NSString *)oldPath {
  NSString *newPath = oldPath;
  if(oldPath != nil) {
    NSRange range = [oldPath rangeOfString:kObsoletePathComponent];
    if (range.length > 0) {
      newPath = [oldPath substringFromIndex:range.location + range.length];
      os_log_debug([KMLogs dataLog], "trimmed keyboard path from '%{public}@' to '%{public}@'", oldPath, newPath);
    }
  }
  return newPath;
}

- (void)migrateSettingsFromKeyman18 {
  os_log_debug([KMLogs dataLog], "migrating settings in UserDefaults from Keyman 18");
  [self migrateSettingsFromKeyman18ToAppGroup];

  // set kDataModelVersion for the current format
  [self writeCurrentDataModelVersion];

  [self removeMigratedInputMethodSettings];
}

/**
 * Used only for setting migration from Keyman 18 to 19
 * Read the settings in the input method's user defaults
 * Write them to the shared app group user defaults
 * Delete them from the input method's user defaults
 */
- (void)migrateSettingsFromKeyman18ToAppGroup {
  NSString *selectedKeyboard = [self.appDefaults stringForKey:kSelectedKeyboardKey];
  if (selectedKeyboard != nil) {
    [self.groupDefaults setObject:selectedKeyboard forKey:kSelectedKeyboardKey];
  }

  NSArray * activeKeyboards = [self.appDefaults arrayForKey:kActiveKeyboardsKey];
  if (activeKeyboards != nil) {
    [self.groupDefaults setObject:activeKeyboards forKey:kEnabledKeyboardsKey];
  }

  if ([self.appDefaults objectForKey:kShowOskOnActivate] != nil) {
    BOOL showOsk = [self.appDefaults boolForKey:kShowOskOnActivate];
    [self.groupDefaults setBool:showOsk forKey:kShowOskOnActivate];
  }
  
  if ([self.appDefaults objectForKey:kForceSentryError] != nil) {
    BOOL forceSentryError = [self.appDefaults boolForKey:kForceSentryError];
    [self.groupDefaults setBool:forceSentryError forKey:kForceSentryError];
  }
  
  NSDictionary * persistedOptions = [self.appDefaults dictionaryForKey:kPersistedOptionsKey];
  if (persistedOptions != nil) {
    [self.groupDefaults setObject:persistedOptions forKey:kPersistedOptionsKey];
  }
}

/**
 * Removes input method settings that have been migrated to the app group.
 * Does not eradicate everything but only those created by Keyman code.
 * For example, the OSK window coordinates, created by NSWindow, must remain.
 */
- (void)removeMigratedInputMethodSettings {
  [self.appDefaults removeObjectForKey:kSelectedKeyboardKey];
  [self.appDefaults removeObjectForKey:kActiveKeyboardsKey];
  [self.appDefaults removeObjectForKey:kShowOskOnActivate];
  [self.appDefaults removeObjectForKey:kForceSentryError];
  [self.appDefaults removeObjectForKey:kPersistedOptionsKey];
  [self.appDefaults removeObjectForKey:kDataModelVersion];
}
@end
