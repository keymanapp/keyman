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
@property (readonly) NSURL *obsoleteKeymanKeyboardsDirectory; // '~/Library/Documents/Keyman-Keyboards'
@end

@implementation KMDataRepository

@synthesize applicationSupportSubDirectory = _applicationSupportSubDirectory;
@synthesize documentsSubDirectory = _documentsSubDirectory;
@synthesize keymanKeyboardsDirectory = _keymanKeyboardsDirectory;
@synthesize obsoleteKeymanKeyboardsDirectory = _obsoleteKeymanKeyboardsDirectory;
@synthesize keymanDataDirectory = _keymanDataDirectory;

NSString *const kKeyboardsDirectoryName = @"Keyman-Keyboards";
/* 
 The name of the subdirectory within '~/Library/Application Support'.
 The convention is to use bundle identifier.
 (Using the subsystem id, "com.keyman.app", is a poor choice because the API
 createDirectoryAtPath sees the .app extension and creates an application file.
 */
NSString *const kKeymanSubdirectoryName = @"keyman.inputmethod.Keyman";

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
  if (_documentsSubDirectory == nil) {
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
  return _documentsSubDirectory;
}

- (NSURL *)applicationSupportSubDirectory {
  if (_applicationSupportSubDirectory == nil) {
    NSError *directoryError = nil;
    
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSURL *applicationSupportUrl = [fileManager URLForDirectory:NSApplicationSupportDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:&directoryError];

    if (directoryError) {
      os_log_error([KMLogs startupLog], "error getting Application Support subdirectory: '%{public}@'", directoryError.localizedDescription);
    } else {
      os_log_info([KMLogs startupLog], "Application Support subdirectory: '%{public}@'", applicationSupportUrl.path);
      _applicationSupportSubDirectory = applicationSupportUrl;
    }
  }
  return _applicationSupportSubDirectory;
}

- (NSURL *)keymanDataDirectory {
  if (_keymanDataDirectory == nil) {
    NSURL *keymanDataUrl = [self.applicationSupportSubDirectory URLByAppendingPathComponent:kKeymanSubdirectoryName  isDirectory: TRUE];
    _keymanDataDirectory = keymanDataUrl;
  }
  return _keymanDataDirectory;
}

- (NSURL *)keymanKeyboardsDirectory {
  if (_keymanKeyboardsDirectory == nil) {
    NSURL *keyboardsUrl = [self.keymanDataDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _keymanKeyboardsDirectory = keyboardsUrl;
  }
  return _keymanKeyboardsDirectory;
}

/**
 * Creates Keyman data directory if it do not exist yet. This is the main data subdirectory: keyman.inputmethod.Keyman
 */
- (void)createDataDirectoryIfNecessary {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:self.keymanDataDirectory.path isDirectory:&isDir];

  if (!exists) {
    NSError *createError = nil;
    os_log_info([KMLogs startupLog], "createDataDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keymanDataDirectory.path);
    [fileManager createDirectoryAtPath:self.keymanDataDirectory.path withIntermediateDirectories:YES attributes:nil error:nil];
    if (createError) {
      os_log_error([KMLogs startupLog], "error creating Keyman data directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs startupLog], "created Keyman data directory: '%{public}@'", self.keymanDataDirectory.path);
    }
  } else {
    os_log_info([KMLogs startupLog], "Keyman data directory already exists: '%{public}@'", self.keymanDataDirectory.path);
  }
}

/**
 * Creates Keyman keyboard directory if it does not exist yet. This is the 'Keyman-Keyboards' directory.
 * It should not be created until after migrating because its existence would block migrating data from the old location.
 */
- (void)createKeyboardsDirectoryIfNecessary {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:self.keymanKeyboardsDirectory.path isDirectory:&isDir];

  if (!exists) {
    NSError *createError = nil;
    os_log_info([KMLogs startupLog], "createKeyboardsDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keymanKeyboardsDirectory.path);
    [fileManager createDirectoryAtPath:self.keymanKeyboardsDirectory.path withIntermediateDirectories:YES attributes:nil error:nil];
    if (createError) {
      os_log_error([KMLogs startupLog], "error creating Keyman-Keyboards directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs startupLog], "created Keyman-Keyboards subdirectory: '%{public}@'", self.keymanKeyboardsDirectory.path);
    }
  } else {
    os_log_info([KMLogs startupLog], "Keyman-Keyboards already exists: '%{public}@'", self.keymanKeyboardsDirectory.path);
  }
}

- (NSURL *)obsoleteKeymanKeyboardsDirectory {
  if (_obsoleteKeymanKeyboardsDirectory == nil) {
    NSURL *keymanUrl = [self.documentsSubDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _obsoleteKeymanKeyboardsDirectory = keymanUrl;
  }
  return _obsoleteKeymanKeyboardsDirectory;
}

- (BOOL)keyboardsExistInDocumentsFolder {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = ([fileManager fileExistsAtPath:self.obsoleteKeymanKeyboardsDirectory.path isDirectory:&isDir]);
  return exists;
}

- (BOOL)migrateData {
  BOOL didMoveData = NO;
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSString *obsoleteKeymanKeyboardsDirectory = self.obsoleteKeymanKeyboardsDirectory.path;
  NSString *dataDirectory = self.keymanDataDirectory.path;
  os_log_info([KMLogs startupLog], "migrateData, move obsoleteKeymanKeyboardsDirectory: '%{public}@' to '%{public}@'", obsoleteKeymanKeyboardsDirectory, dataDirectory);

  BOOL isDir;
  BOOL dataDirectoryExistsInNewLocation = ([fileManager fileExistsAtPath:self.keymanDataDirectory.path isDirectory:&isDir]);
  os_log([KMLogs startupLog], "data directory exists in new location, %{public}@: %{public}@", self.keymanDataDirectory.path, dataDirectoryExistsInNewLocation?@"YES":@"NO");

  BOOL keyboardsDirectoryExistsInNewLocation = ([fileManager fileExistsAtPath:self.keymanKeyboardsDirectory.path isDirectory:&isDir]);
  os_log([KMLogs startupLog], "keyboards directory exists in new location, %{public}@: %{public}@", self.keymanKeyboardsDirectory.path, keyboardsDirectoryExistsInNewLocation?@"YES":@"NO");

  BOOL dataExistsInOldLocation = [self keyboardsExistInDocumentsFolder];
  os_log([KMLogs startupLog], "obsolete keyman keyboards directory exists: %@", dataExistsInOldLocation?@"YES":@"NO");

  if (dataExistsInOldLocation) {
    NSError *moveError = nil;
    didMoveData = [fileManager moveItemAtURL:self.obsoleteKeymanKeyboardsDirectory
                         toURL:self.keymanKeyboardsDirectory
                         error:&moveError];
    if (moveError) {
      os_log_error([KMLogs startupLog], "error migrating data: '%{public}@'", moveError.localizedDescription);
    } else {
      os_log_error([KMLogs startupLog], "data migrated successfully to: '%{public}@'", self.keymanKeyboardsDirectory.path);
    }
  }
  
  return didMoveData;
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
