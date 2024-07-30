/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMResourcesRepository.m
 * Keyman
 *
 * Created by Shawn Schantz on 2024-07-30.
 *
 * Singleton object which serves as an abstraction for the reading and writing of Keyman data.
 * The 'data' currently consists of keyman keyboards installed by the user. All data is saved locally on disk using NSFileManager.
 * This is in contrast with the lighter weight Settings which is stored using UserDefaults and handled by KMSettingsRepository.
 */

#import "KMDataRepository.h"
#import "KMLogs.h"

@implementation KMDataRepository

NSString* _obsoleteKeymanDataDirectory = nil;
NSString* _keymanDataDirectory = nil;

+ (KMDataRepository *)shared
{
  static KMDataRepository *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMDataRepository alloc] init];
  });
  return shared;
}

- (void)migrateResources {
  NSError *directoryError = nil;
  
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSURL *applicationSupportUrl = [fileManager URLForDirectory:NSApplicationSupportDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:&directoryError];
  
  os_log_info([KMLogs startupLog], "applicationSupportDirectory: '%{public}@'", applicationSupportUrl);

  NSString *appId = [[NSBundle mainBundle] bundleIdentifier];
  os_log_info([KMLogs startupLog], "application bundleIdentifier: '%{public}@'", appId);
  
  NSURL *keymanUrl = [applicationSupportUrl URLByAppendingPathComponent: appId isDirectory: TRUE];

  //NSURL *keymanUrl = [NSURL fileURLWithPath:appId isDirectory:YES relativeToURL:applicationSupportUrl];

  os_log_info([KMLogs startupLog], "keymanUrl: '%{public}@'", keymanUrl);
  // returns -> '/Users/sgschantz/Library/Application Support'

  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:keymanUrl.path isDirectory:&isDir];
  
  if (exists) {
    os_log_info([KMLogs startupLog], "keymanUrl '%{public}@' exists", keymanUrl);
      if (isDir) {
        os_log_info([KMLogs startupLog], "keymanUrl '%{public}@' is a directory", keymanUrl);
      }
  } else {
    os_log_info([KMLogs startupLog], "keymanUrl '%{public}@' does not exist", keymanUrl);
  }
}

/**
 * Locate and create the Keyman data path; currently in ~/Documents/Keyman-Keyboards
 */
- (NSString *)_obsoleteKeymanDataDirectory {
  if(_keymanDataDirectory == nil) {
    NSString *documentDirPath = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
    _keymanDataDirectory = [documentDirPath stringByAppendingPathComponent:@"Keyman-Keyboards"];

    os_log_debug([KMLogs dataLog], "creating keymanDataPath, %{public}@", _keymanDataDirectory);

    NSFileManager *fm = [NSFileManager defaultManager];
    if (![fm fileExistsAtPath:_keymanDataDirectory]) {
      [fm createDirectoryAtPath:_keymanDataDirectory withIntermediateDirectories:YES attributes:nil error:nil];
    }
  }
  return _keymanDataDirectory;
}

- (NSString *)keymanDataPath {
  return _keymanDataDirectory;
}

@end
