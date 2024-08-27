/*
 * Keyman is copyright (C) SIL International. MIT License.
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
@property (readonly) NSURL *applicationSupportSubDirectory;
@property (readonly) NSURL *documentsSubDirectory;
@property (readonly) NSURL *obsoleteKeymanKeyboardsDirectory;
@end

@implementation KMDataRepository
/**
 * Two directory trees are represented by the following properties, one in active use
 * and one that is obsolete.
 * The actively used directories, begin with the parent
 *    applicationSupportSubDirectory: '~/Library/Application Support'
 *      keymanDataDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
 *        keymanKeyboardsDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
 * The obsolete directories, begin with the parent
 *    documentsSubDirectory: '~/Documents'
 *      obsoleteKeymanKeyboardsDirectory: '~/Documents/Keyman-Keyboards'
 */
@synthesize applicationSupportSubDirectory = _applicationSupportSubDirectory;
@synthesize keymanDataDirectory = _keymanDataDirectory;
@synthesize keymanKeyboardsDirectory = _keymanKeyboardsDirectory;
@synthesize documentsSubDirectory = _documentsSubDirectory;
@synthesize obsoleteKeymanKeyboardsDirectory = _obsoleteKeymanKeyboardsDirectory;

NSString *const kKeyboardsDirectoryName = @"Keyman-Keyboards";
/**
 * The name of the subdirectory within '~/Library/Application Support'.
 * We follow the convention of using the bundle identifier rather than our subsystem id.
 * (Also, using the subsystem id, "com.keyman.app", is a poor choice because the API
 * createDirectoryAtPath sees the .app extension and creates an application file.)
 */
NSString *const kKeymanSubdirectoryName = @"keyman.inputmethod.Keyman";

+ (KMDataRepository *)shared {
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
      os_log_error([KMLogs dataLog], "error getting Documents subdirectory: '%{public}@'", directoryError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "Documents subdirectory: '%{public}@'", documentsUrl);
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
      os_log_error([KMLogs dataLog], "error getting Application Support subdirectory: '%{public}@'", directoryError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "Application Support subdirectory: '%{public}@'", applicationSupportUrl.path);
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
    os_log_info([KMLogs dataLog], "createDataDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keymanDataDirectory.path);
    [fileManager createDirectoryAtPath:self.keymanDataDirectory.path withIntermediateDirectories:YES attributes:nil error:nil];
    if (createError) {
      os_log_error([KMLogs dataLog], "error creating Keyman data directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "created Keyman data directory: '%{public}@'", self.keymanDataDirectory.path);
    }
  } else {
    os_log_info([KMLogs dataLog], "Keyman data directory already exists: '%{public}@'", self.keymanDataDirectory.path);
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
    os_log_info([KMLogs dataLog], "createKeyboardsDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keymanKeyboardsDirectory.path);
    [fileManager createDirectoryAtPath:self.keymanKeyboardsDirectory.path withIntermediateDirectories:YES attributes:nil error:nil];
    if (createError) {
      os_log_error([KMLogs dataLog], "error creating Keyman-Keyboards directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "created Keyman-Keyboards subdirectory: '%{public}@'", self.keymanKeyboardsDirectory.path);
    }
  } else {
    os_log_info([KMLogs dataLog], "Keyman-Keyboards already exists: '%{public}@'", self.keymanKeyboardsDirectory.path);
  }
}

- (NSURL *)obsoleteKeymanKeyboardsDirectory {
  if (_obsoleteKeymanKeyboardsDirectory == nil) {
    NSURL *keymanUrl = [self.documentsSubDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _obsoleteKeymanKeyboardsDirectory = keymanUrl;
  }
  return _obsoleteKeymanKeyboardsDirectory;
}

/**
 *  Only called from migrateData.
 *  Causes user to be prompted for permission to access ~/Documents, but they should already have it.
 *  otherwise we would not be attempting to migrate.
 */
- (BOOL)keyboardsExistInObsoleteDirectory {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = ([fileManager fileExistsAtPath:self.obsoleteKeymanKeyboardsDirectory.path isDirectory:&isDir]);
  return exists;
}

/**
 * Migrate the keyboards data from the old location in '~/Documents' to the new location '~/Application Support/keyman.inputmethod.Keyman/'
 * This should only be called if the Keyman settings written to the UserDefaults indicates that we have data in the old location.
 * If this is the case, then we expect the user to have already granted permission for Keyman to access the ~/Documents directory.
 * If that permission has been removed for some reason, then calling this code will cause the user to be asked for permission again.
 */
- (BOOL)migrateData {
  BOOL didMoveData = NO;
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL dataExistsInOldLocation = [self keyboardsExistInObsoleteDirectory];
  os_log([KMLogs dataLog], "obsolete keyman keyboards directory exists: %@", dataExistsInOldLocation?@"YES":@"NO");

  // only move data if there is something to move
  if (dataExistsInOldLocation) {
    NSError *moveError = nil;
    didMoveData = [fileManager moveItemAtURL:self.obsoleteKeymanKeyboardsDirectory
                         toURL:self.keymanKeyboardsDirectory
                         error:&moveError];
    if (moveError) {
      os_log_error([KMLogs dataLog], "data migration failed: '%{public}@'", moveError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "data migrated successfully to: '%{public}@'", self.keymanKeyboardsDirectory.path);
    }
  }
  
  return didMoveData;
}

@end
