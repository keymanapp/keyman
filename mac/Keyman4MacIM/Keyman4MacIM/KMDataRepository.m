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

// the name of the app group shared by the Keyman input method and the Keyman configuration app
// an iOS-style app group of this name is defined for Keyman on developer.apple.com and linked
// to the each app's provisioning profile
NSString *const kKeymanGroupId = @"group.com.keyman";

@interface KMDataRepository ()
@property (readonly) NSURL *applicationSupportSubDirectory;
@property (readonly) NSURL *documentsSubDirectory;
@property (readonly) NSURL *keyman17KeyboardsDirectory;
@end

@implementation KMDataRepository
/**
 * Three directory trees are represented by the following properties, one in active use
 * and two that are obsolete.
 * The actively used directories, introduced in Keyman 19, are shared via the app group `group.com.keyman`:
 *  'Group Containers/group.com.keyman/Library/Application Support/Keyman-Packages/
 * The obsolete directories from Keyman 18 are:
 *    applicationSupportSubDirectory: '~/Library/Application Support'
 *      keyman18DataDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman'
 *        keyman18KeyboardsDirectory: '~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards'
 * The obsolete directories from Keyman 17 and earlier are:
 *    documentsSubDirectory: '~/Documents'
 *      keyman17KeyboardsDirectory: '~/Documents/Keyman-Keyboards'
 */
@synthesize applicationSupportSubDirectory = _applicationSupportSubDirectory;
@synthesize keyman18DataDirectory = _keyman18DataDirectory;
@synthesize keyman18KeyboardsDirectory = _keyman18KeyboardsDirectory;
@synthesize documentsSubDirectory = _documentsSubDirectory;
@synthesize keyman17KeyboardsDirectory = _keyman17KeyboardsDirectory;

@synthesize keyman19ContainerDirectory = _keyman19ContainerDirectory;
@synthesize keyman19KeyboardsDirectory = _keyman19KeyboardsDirectory;

NSString *const kKeyboardsDirectoryName = @"Keyman-Keyboards";
/**
 * The name of the subdirectory within '~/Library/Application Support'.
 * We follow the convention of using the bundle identifier rather than our subsystem id.
 * (Also, using the subsystem id, "com.keyman.app", is a poor choice because the API
 * createDirectoryAtPath sees the .app extension and creates an application file.)
 */
NSString *const kKeymanSubdirectoryName = @"keyman.inputmethod.Keyman";

NSString *const kContainerKeyboardsPartialPath = @"Library/Application Support/Keyman-Packages";

+ (KMDataRepository *)shared {
  static KMDataRepository *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMDataRepository alloc] init];
  });
  return shared;
}

/**
 * Returns true if URL is an existing directory that is not empty
 */
+ (BOOL)isExistingNonEmptyDirectory:(NSURL *)directoryUrl {
  return [KMDataRepository isExistingDirectory: directoryUrl] && ![KMDataRepository isEmptyDirectory: directoryUrl];
}

/**
 * Returns true if URL is an existing directory that is empty
 */
+ (BOOL)isExistingEmptyDirectory:(NSURL *)directoryUrl {
  return [KMDataRepository isExistingDirectory: directoryUrl] && [KMDataRepository isEmptyDirectory: directoryUrl];
}

/**
 * Returns true if the directory is empty
 */
+ (BOOL)isExistingDirectory:(NSURL *)directoryUrl {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL nonEmptyDirectoryExists = false;
  BOOL isDirectory;
  BOOL urlExists = ([fileManager fileExistsAtPath:directoryUrl.path isDirectory:&isDirectory]);
  return urlExists && isDirectory;
}

/**
 * Returns true if the directory is empty
 */
+ (BOOL)isEmptyDirectory:(NSURL *)directoryUrl {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isEmpty = true;
  
  NSError *error = nil;
  NSArray *contents = [fileManager contentsOfDirectoryAtPath:directoryUrl.path error:&error];
  
  if (error) {
    os_log_debug([KMLogs dataLog], "cannot read directory: %@", directoryUrl.path);
  } else if (contents.count > 0) {
    isEmpty = false;
  } else {
    isEmpty = true;
  }
  
  return isEmpty;
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

- (NSURL *)keyman18DataDirectory {
  if (_keyman18DataDirectory == nil) {
    NSURL *keymanDataUrl = [self.applicationSupportSubDirectory URLByAppendingPathComponent:kKeymanSubdirectoryName  isDirectory: TRUE];
    _keyman18DataDirectory = keymanDataUrl;
  }
  return _keyman18DataDirectory;
}

- (NSURL *)keyman18KeyboardsDirectory {
  if (_keyman18KeyboardsDirectory == nil) {
    NSURL *keyboardsUrl = [self.keyman18DataDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _keyman18KeyboardsDirectory = keyboardsUrl;
  }
  return _keyman18KeyboardsDirectory;
}

- (NSURL *)keyman19ContainerDirectory {
  if (_keyman19ContainerDirectory == nil) {
    
    NSFileManager *fileManager = [NSFileManager defaultManager];
    os_log_info([KMLogs dataLog], "about to fetch containerUrl");
    NSURL *containerUrl = [fileManager containerURLForSecurityApplicationGroupIdentifier: kKeymanGroupId];
    os_log_info([KMLogs dataLog], "containerUrl: '%{public}@'", containerUrl.path);

    _keyman19ContainerDirectory = containerUrl;
  }
  return _keyman19ContainerDirectory;
}

- (NSURL *)keyman19KeyboardsDirectory {
  if (_keyman19KeyboardsDirectory == nil) {
    NSURL *keyboardsUrl = [self.keyman19ContainerDirectory URLByAppendingPathComponent:kContainerKeyboardsPartialPath  isDirectory: TRUE];
    _keyman19KeyboardsDirectory = keyboardsUrl;
  }
  return _keyman19KeyboardsDirectory;
}

/**
 * Creates Keyman 19 data directory if it does not exist yet. This is under 'Group Containers'.
 */
- (void)createKeyman19SharedDirectoriesIfNecessary {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:self.keyman19KeyboardsDirectory.path isDirectory:&isDir];

  if (!exists) {
    NSError *createError = nil;
    os_log_info([KMLogs dataLog], "createKeyman19SharedDirectoriesIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keyman19KeyboardsDirectory.path);
    [fileManager createDirectoryAtPath:self.keyman19KeyboardsDirectory.path withIntermediateDirectories:YES attributes:nil error:&createError];
    if (createError) {
      os_log_error([KMLogs dataLog], "error creating Keyman data directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "created Keyman data directory: '%{public}@'", self.keyman19KeyboardsDirectory.path);
    }
  } else {
    // TODO: remove test code
    os_log_info([KMLogs dataLog], "Keyman data directory already exists: '%{public}@'", self.keyman19KeyboardsDirectory.path);
    NSString *contentString = @"Not a .kmp file";
    NSString *filePath = [self.keyman19KeyboardsDirectory.path stringByAppendingPathComponent:@"readable.txt"];
    NSData *data = [contentString dataUsingEncoding:NSUTF8StringEncoding];
    if ([fileManager createFileAtPath: filePath contents:data attributes:nil]) {
      os_log_info([KMLogs dataLog], "created file at: %{public}@", filePath);
    } else {
      os_log_info([KMLogs dataLog], "failed to create file at: %{public}@", filePath);
    }
  }
}

// TODO: remove once migration to Keyman 19 data directory is complete
/**
 * Creates Keyman data directory if it does not exist yet. This is the main data subdirectory: keyman.inputmethod.Keyman
 */
- (void)createKeyman18DataDirectoryIfNecessary {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:self.keyman18DataDirectory.path isDirectory:&isDir];

  if (!exists) {
    NSError *createError = nil;
    os_log_info([KMLogs dataLog], "createDataDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keyman18DataDirectory.path);
    [fileManager createDirectoryAtPath:self.keyman18DataDirectory.path withIntermediateDirectories:YES attributes:nil error:&createError];
    if (createError) {
      os_log_error([KMLogs dataLog], "error creating Keyman data directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "created Keyman data directory: '%{public}@'", self.keyman18DataDirectory.path);
    }
  } else {
    os_log_info([KMLogs dataLog], "Keyman data directory already exists: '%{public}@'", self.keyman18DataDirectory.path);
  }
}

/**
 * Creates Keyman keyboard directory if it does not exist yet. This is the 'Keyman-Keyboards' directory.
 * It should not be created until after migrating because its existence would block migrating data from the old location.
 */
- (void)createKeyboardsDirectoryIfNecessary {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
  BOOL exists = [fileManager fileExistsAtPath:self.keyman18KeyboardsDirectory.path isDirectory:&isDir];

  if (!exists) {
    NSError *createError = nil;
    os_log_info([KMLogs dataLog], "createKeyboardsDirectoryIfNecessary, about to attempt createDirectoryAtPath for: '%{public}@'", self.keyman18KeyboardsDirectory.path);
    [fileManager createDirectoryAtPath:self.keyman18KeyboardsDirectory.path withIntermediateDirectories:YES attributes:nil error:&createError];
    if (createError) {
      os_log_error([KMLogs dataLog], "error creating Keyman-Keyboards directory: '%{public}@'", createError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "created Keyman-Keyboards subdirectory: '%{public}@'", self.keyman18KeyboardsDirectory.path);
    }
  } else {
    os_log_info([KMLogs dataLog], "Keyman-Keyboards already exists: '%{public}@'", self.keyman18KeyboardsDirectory.path);
  }
}

- (NSURL *)keyman17KeyboardsDirectory {
  if (_keyman17KeyboardsDirectory == nil) {
    NSURL *keymanUrl = [self.documentsSubDirectory URLByAppendingPathComponent:kKeyboardsDirectoryName  isDirectory: TRUE];
    _keyman17KeyboardsDirectory = keymanUrl;
  }
  return _keyman17KeyboardsDirectory;
}

/**
 *  Only called from migrateDataForKeyman18.
 *  Causes user to be prompted for permission to access ~/Documents, but they should already have it.
 *  otherwise we would not be attempting to migrate.
 */
- (BOOL)keyboardsExistInDocumentsDirectory {
  return [KMDataRepository isExistingNonEmptyDirectory: self.keyman17KeyboardsDirectory];
}

/**
 *  Only called from migrateDataForKeyman19.
 *  Checks to see if the keyboards directory exists in '~/Library/Application Support/keyman.inputmethod.Keyman' and is not empty
 */
- (BOOL)keyboardsExistInInputMethodDataDirectory {
  return [KMDataRepository isExistingNonEmptyDirectory: self.keyman18KeyboardsDirectory];
}

/**
 *  Checks to see if the keyboards directory exists in '~/Library/Group Containers/group.com.keyman/Library/Application Support' and is not empty
 */
- (BOOL)keyboardsExistInGroupContainerDirectory {
  return [KMDataRepository isExistingNonEmptyDirectory: self.keyman19KeyboardsDirectory];
}

// TODO: delete - no reason to move data more than once
/**
 * Migrate the keyboards data from the old location in '~/Documents' to the location '~/Library/Application Support/keyman.inputmethod.Keyman'
 * This should only be called if the Keyman settings written to the UserDefaults indicates that we have data in the old location.
 * If this is the case, then we expect the user to have already granted permission for Keyman to access the ~/Documents directory.
 * If that permission has been removed for some reason, then calling this code will cause the user to be asked for permission again.
 */
- (BOOL)migrateDataForKeyman18 {
  BOOL didMoveData = NO;
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL dataExistsInOldLocation = [self keyboardsExistInDocumentsDirectory];
  os_log_debug([KMLogs dataLog], "keyman 17 keyboards directory exists: %@", dataExistsInOldLocation?@"YES":@"NO");

  // only move data if there is something to move
  if (dataExistsInOldLocation) {
    NSError *moveError = nil;
    didMoveData = [fileManager moveItemAtURL:self.keyman17KeyboardsDirectory
                         toURL:self.keyman18KeyboardsDirectory
                         error:&moveError];
    if (moveError) {
      os_log_error([KMLogs dataLog], "data migration failed: '%{public}@'", moveError.localizedDescription);
    } else {
      os_log_info([KMLogs dataLog], "data migrated successfully to: '%{public}@'", self.keyman18KeyboardsDirectory.path);
    }
  }
  
  return didMoveData;
}

/**
 * Move all the packages contained in the directory
 */
+ (void)movePackages: (NSURL*)sourceDirectory to: (NSURL*)destinationDirectory {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSArray *keys = @[NSURLIsDirectoryKey];

  // fetch the individual keyboard package directories within the keyboards directory
  NSArray *packageDirectories = [fileManager contentsOfDirectoryAtURL:sourceDirectory includingPropertiesForKeys:keys options:NSDirectoryEnumerationSkipsHiddenFiles error:nil];

  // Iterate and filter for directories
  for (NSURL *packageUrl in packageDirectories) {
      NSNumber *isDirectory;
      [packageUrl getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
      
      if ([isDirectory boolValue]) {
        NSString *packageName = packageUrl.lastPathComponent;
        NSURL *packageSourceUrl = [sourceDirectory URLByAppendingPathComponent:packageName  isDirectory: true];
        NSURL *packageDestinationUrl = [destinationDirectory URLByAppendingPathComponent:packageName  isDirectory: true];

        os_log_info([KMLogs dataLog], "moving package directory: '%{public}@'", packageName);
        BOOL movedDirectory = [KMDataRepository moveDirectory: packageSourceUrl to: packageDestinationUrl];
      }
  }
}

/**
 * Move a single directory
 */
+ (BOOL)moveDirectory: (NSURL*)sourceDirectory to: (NSURL*)destinationDirectory {
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSError *moveError = nil;
  BOOL didMoveData = [fileManager moveItemAtURL:sourceDirectory
                                      toURL:destinationDirectory
                                      error:&moveError];
  if (moveError) {
    os_log_error([KMLogs dataLog], "data migration failed: '%{public}@'", moveError.localizedDescription);
  } else {
    os_log_info([KMLogs dataLog], "data migrated successfully to: '%{public}@'", destinationDirectory.path);
  }
  return didMoveData;
}

/**
 * Migrate the keyboards data from the input method specific location in '~/Application Support/keyman.inputmethod.Keyman/'
 * to the shared location in '~/Library/Group Containers/group.com.keyman/Library/Application Support'
 * This should only be called if the Keyman settings written to the UserDefaults indicates that we have data in the old location.
 */
- (BOOL)migrateDataForKeyman19 {
  BOOL didMoveData = NO;
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL dataExistsInOldLocation = [self keyboardsExistInInputMethodDataDirectory];
  os_log_debug([KMLogs dataLog], "keyman 18 keyboards directory exists: %@", dataExistsInOldLocation?@"YES":@"NO");

  // only move data if there is something to move
  if (dataExistsInOldLocation) {

    [KMDataRepository.shared createKeyman19SharedDirectoriesIfNecessary];
    [KMDataRepository movePackages:[self keyman18KeyboardsDirectory] to:[self keyman19KeyboardsDirectory]];
  }
  
  return didMoveData;
}


- (NSString*)buildFullPath:(NSString *)fromPartialPath {
  NSString *fullPath = [self.keyman18KeyboardsDirectory.path stringByAppendingString:fromPartialPath];
  os_log_debug([KMLogs dataLog], "buildFullPath: '%{public}@' fromPartialPath '%{public}@'",
               fullPath, fromPartialPath);
  return fullPath;
}

- (NSString *)trimToPartialPath:(NSString *)fromFullPath {
  NSString *partialPath = fromFullPath;
  if(fromFullPath != nil) {
    NSRange range = [fromFullPath rangeOfString:kKeyboardsDirectoryName];
    if (range.length > 0) {
      partialPath = [fromFullPath substringFromIndex:range.location + range.length];
      os_log_debug([KMLogs dataLog], "trimToPartialPath: fromFullPath: '%{public}@' to partialPath: '%{public}@'", fromFullPath, partialPath);
    }
  }
  return partialPath;
}

- (NSString *)buildPartialPathFrom:(NSString *)keyboardSubdirectory keyboardFile:(NSString *)kmxFilename {
  NSMutableArray *pathComponents = [[NSMutableArray alloc] initWithCapacity:0];
  [pathComponents addObject:@"/"];
  [pathComponents addObject:keyboardSubdirectory];
  [pathComponents addObject:kmxFilename];
  NSString *keyboardPartialPath = [NSString pathWithComponents:pathComponents];
  os_log_debug([KMLogs keyboardLog], "buildPartialPathFrom, keyboardSubdirectory: %{public}@, kmxFileName: %{public}@, keyboardPartialPath : %{public}@",
               keyboardSubdirectory, kmxFilename, keyboardPartialPath);
  return keyboardPartialPath;
}


@end
