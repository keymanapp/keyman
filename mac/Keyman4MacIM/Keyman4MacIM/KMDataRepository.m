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

@interface KMDataRepository ()
@property (readonly) NSURL *applicationSupportSubDirectory;   // '~/Library/Application Support'
@property (readonly) NSURL *documentsSubDirectory;    // '~/Documents'
@property (readonly) NSURL *keymanDataDirectory;      // '~/Library/Application Support/com.keyman.app'
@property (readonly) NSURL *keymanKeyboardsDirectory;
// keymanKeyboardsDirectory = '~/Library/Application Support/com.keyman.app/Keyman-Keyboards'
@property (readonly) NSURL *obsoleteKeymanDataDirectory; // '~/Library/Documents/Keyman-Keyboards'
@end

@implementation KMDataRepository

@synthesize applicationSupportSubDirectory = _applicationSupportSubDirectory;
@synthesize documentsSubDirectory = _documentsSubDirectory;
@synthesize keymanKeyboardsDirectory = _keymanKeyboardsDirectory;
@synthesize obsoleteKeymanDataDirectory = _obsoleteKeymanDataDirectory;
@synthesize keymanDataDirectory = _keymanDataDirectory;

NSString *const kKeyboardsDirectoryName = @"Keyman-Keyboards";
/* 
 name of the subdirectory within '~/Library/Application Support'
 the convention is to use bundle identifier ("keyman.inputmethod.Keyman")
 but we'll use this name, which matches our logging subsystem
 */
NSString *const kKeymanSubdirectoryName = @"com.keyman.app";
/*
NSString* _obsoleteKeymanDataDirectory = nil;
NSString* _keymanDataDirectory = nil;
NSURL* _ApplicationSupportSubDirectory = nil;
NSURL* _DocumentsSubDirectory = nil;
*/

+ (KMDataRepository *)shared
{
  static KMDataRepository *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMDataRepository alloc] init];
  });
  return shared;
}

- (NSURL *)documentsSubDirectory {
  if (self.documentsSubDirectory == nil) {
    NSError *directoryError = nil;
    
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSURL *documentsUrl = [fileManager URLForDirectory:NSDocumentDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:&directoryError];
    
    if (directoryError) {
      os_log_error([KMLogs startupLog], "error getting Documents subdirectory: '%{public}@'", directoryError.localizedDescription);
    } else {
      os_log_info([KMLogs startupLog], "Documents subdirectory: '%{public}@'", documentsUrl);
      _documentsSubDirectory = documentsUrl;
    }
  }
  return self.documentsSubDirectory;
}

- (NSURL *)applicationSupportSubDirectory {
  if (self.applicationSupportSubDirectory == nil) {
    NSError *directoryError = nil;
    
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSURL *applicationSupportUrl = [fileManager URLForDirectory:NSApplicationSupportDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:&directoryError];

    if (directoryError) {
      os_log_error([KMLogs startupLog], "error getting Application Support subdirectory: '%{public}@'", directoryError.localizedDescription);
    } else {
      os_log_info([KMLogs startupLog], "Application Support subdirectory: '%{public}@'", applicationSupportUrl);
      _applicationSupportSubDirectory = applicationSupportUrl;
    }
  }
  return self.applicationSupportSubDirectory;
}

- (NSURL *)keymanDataDirectory {
  if (self.keymanDataDirectory == nil) {
    NSURL *keymanDataUrl = [self.applicationSupportSubDirectory URLByAppendingPathComponent:kKeymanSubdirectoryName  isDirectory: TRUE];
    _keymanDataDirectory = keymanDataUrl;
  }
  return self.keymanDataDirectory;
}

- (NSURL *)keymanKeyboardsDirectory {
  if (self.keymanKeyboardsDirectory == nil) {
    NSURL *keyboardsUrl = [self.keymanDataDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _keymanKeyboardsDirectory = keyboardsUrl;
  }
  return self.keymanKeyboardsDirectory;
}

- (NSURL *)obsoleteKeymanDataDirectory {
  if (self.obsoleteKeymanDataDirectory == nil) {
    NSURL *keymanUrl = [self.documentsSubDirectory URLByAppendingPathComponent:kKeymanSubdirectoryName  isDirectory: TRUE];
    _obsoleteKeymanDataDirectory = keymanUrl;
  }
  return self.obsoleteKeymanDataDirectory;
}

- (void)migrateResources {
  os_log_info([KMLogs startupLog], "keymanKeyboardsDirectory: '%{public}@'", self.keymanKeyboardsDirectory);
  os_log_info([KMLogs startupLog], "obsoleteKeymanDataDirectory: '%{public}@'", self.obsoleteKeymanDataDirectory);

  /*
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
   */
}

/**
 * Locate and create the Keyman data path; currently in ~/Documents/Keyman-Keyboards
 */
/*
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

- (NSString *)keymanDataDirectory {
  return _keymanDataDirectory;
}
*/
@end
