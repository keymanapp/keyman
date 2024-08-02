/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMPackageReader.m
 * Keyman
 *
 * Created by Shawn Schantz on 2021/12/06.
 *
 * Read package information from kmp.json, if it exists. If not, read from kmp.inf.
 * Then create and return KMPackageInfo object.
 *
 */

#import "KMPackageReader.h"
#import "KMLogs.h"

static NSString *const kPackageJsonFile = @"kmp.json";
static NSString *const kPackageInfFile = @"kmp.inf";

static NSString *const kPackage = @"[Package]";
static NSString *const kButtons = @"[Buttons]";
static NSString *const kStartMenu = @"[StartMenu]";
static NSString *const kStartMenuEntries = @"[StartMenuEntries]";
static NSString *const kInfo = @"[Info]";
static NSString *const kFiles = @"[Files]";

static NSString *const kAuthor = @"Author";
static NSString *const kCopyright = @"Copyright";
static NSString *const kFile = @"File";
static NSString *const kFont = @"Font";
static NSString *const kGraphicFile = @"GraphicFile";
static NSString *const kKeyboard = @"Keyboard";
static NSString *const kName = @"Name";
static NSString *const kReadMeFile = @"ReadMeFile";
static NSString *const kVersion = @"Version";
static NSString *const kWebSite = @"WebSite";
static NSString *const kWelcome = @"Welcome";

typedef enum {
  ctPackage, ctButtons, ctStartMenu, ctStartMenuEntries, ctInfo, ctFiles, ctUnknown
} ContentType;

@implementation KMPackageReader

- (instancetype)init {
  self = [super init];
  if (self) {
    _debugMode = NO;
  }
  return self;
}

/**
 * Attempt to load Keyman Package Information from kmp.json.
 * If kmp.json file is not found or not readable, load from legacy kmp.inf file.
 * @param  <path>  the full path of the package folder.
 */
- (KMPackageInfo *)loadPackageInfo:(NSString *)path {
  KMPackageInfo *packageInfo = nil;
  NSString *jsonFilename = [path stringByAppendingPathComponent:kPackageJsonFile];
  
  packageInfo = [self loadPackageInfoFromJsonFile:jsonFilename];
  
  if (packageInfo == nil) {
    NSString *infoFilename = [path stringByAppendingPathComponent:kPackageInfFile];
    packageInfo = [self loadPackageInfoFromInfFile:infoFilename];
  }
  
  return packageInfo;
}

/*
 // TODO: not used, delete
- (NSString *)packageNameFromPackageInfo:(NSString *)path {
  NSString *packageName = nil;
  
  KMPackageInfo *packageInfo = [self loadPackageInfo:path];
  packageName = packageInfo.packageName;
  
  return packageName;
}
*/

/**
 * read JSON file and load it into KMPackageInfo object
 * returns nil if the file does not exist or it cannot be parsed as JSON
 */
- (KMPackageInfo *) loadPackageInfoFromJsonFile:(NSString *)path {
  KMPackageInfo * packageInfo = nil;
  
  os_log([KMLogs dataLog], "Loading JSON package info from path: %{public}@", path);
  NSError *readError = nil;
  NSData *data = [NSData dataWithContentsOfFile:path options:kNilOptions error:&readError];
  NSDictionary *parsedData = nil;
  
  if (data == nil) {
    os_log_error([KMLogs dataLog], "Error reading JSON file at path : %{public}@, error: %{public}@ ", path, readError);
  } else {
    NSError *parseError = nil;
    parsedData = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&parseError];
    
    if (parsedData == nil) {
      os_log_error([KMLogs dataLog], "Error parsing JSON file at path : %{public}@, error: %{public}@ ", path, parseError);
    } else {
      packageInfo = [self createPackageInfoFromJsonData:parsedData];
    }
  }
  
  return packageInfo;
}

/**
 * Creates a KeyboardInfo object using data loaded from kmp.json.
 * @param keyboard  an NSDictionary containing all the key-value pairs of a keyboard object from the kmp.json keyboards array
 * @return the corresponding KeyboardInfo value object
 */
- (KMKeyboardInfo *) createKeyboardInfoFromJsonKeyboard:(NSDictionary *) keyboard {
  
  KMKeyboardInfoBuilder *builder = [[KMKeyboardInfoBuilder alloc] init];
  
  NSArray *languages = keyboard[@"languages"];
  NSMutableArray *languageInfoArray = [NSMutableArray arrayWithCapacity:0];
  
  for (NSDictionary *language in languages) {
    KMLanguageInfo *languageInfo = [[KMLanguageInfo alloc] initWithName:language[@"name"]
                                                             identifier:language[@"id"]];
    
    [languageInfoArray addObject:languageInfo];
  }
  
  builder.name = keyboard[@"name"];
  builder.identifier = keyboard[@"id"];
  builder.version = keyboard[@"version"];
  builder.oskFont = keyboard[@"oskFont"];
  builder.displayFont = keyboard[@"displayFont"];
  builder.languages = [languageInfoArray copy];
  
  KMKeyboardInfo *keyboardInfo = [[KMKeyboardInfo alloc] initWithBuilder:builder];
  return keyboardInfo;
}

- (KMPackageInfo *) createPackageInfoFromJsonData:(NSDictionary *) jsonData {
  
  NSMutableArray *keyboardInfoArray = [NSMutableArray arrayWithCapacity:0];
  NSMutableArray *fontArray = [NSMutableArray arrayWithCapacity:0];
  NSArray *keyboards = jsonData[@"keyboards"];
  
  // loop through keyboards array to add keyboards,
  // then loop through each keyboard's languages array to add languages
  
  for (NSDictionary *keyboard in keyboards) {
    KMKeyboardInfo *keyboardInfo = [self createKeyboardInfoFromJsonKeyboard:keyboard];
    
    // add oskFont to fontArray if not nil or empty string
    if ([keyboardInfo.oskFont length]) {
      [fontArray addObject:keyboardInfo.oskFont];
    }
    
    // add displayFont to fontArray if not not nil or empty string and not equal to oskFont
    if ([keyboardInfo.displayFont length] &&
        ![keyboardInfo.displayFont isEqualToString:keyboardInfo.displayFont]) {
      [fontArray addObject:keyboardInfo.displayFont];
    }
    
    [keyboardInfoArray addObject:keyboardInfo];
  }
  
  KMPackageInfoBuilder *builder =
  [[KMPackageInfoBuilder alloc] init];
  
  builder.packageName = jsonData[@"info"][@"name"][@"description"];
  builder.packageVersion = jsonData[@"info"][@"version"][@"description"];
  builder.readmeFilename = jsonData[@"options"][@"readmeFile"];
  builder.graphicFilename = jsonData[@"options"][@"graphicFile"];
  builder.fileVersion = jsonData[@"system"][@"fileVersion"];
  builder.keymanDeveloperVersion = jsonData[@"system"][@"keymanDeveloperVersion"];
  builder.copyright = jsonData[@"info"][@"copyright"][@"description"];
  builder.authorName = jsonData[@"info"][@"author"][@"description"];
  builder.authorUrl = jsonData[@"info"][@"author"][@"url"];
  builder.website = jsonData[@"info"][@"website"][@"url"];
  builder.keyboards = [keyboardInfoArray copy];
  builder.fonts = [fontArray copy];
  
  KMPackageInfo *packageInfo = [[KMPackageInfo alloc] initWithBuilder:builder];
  
  return packageInfo;
}

// TODO: refactor, break up and eliminate duplicate code
/**
 * Creates a KMPackageInfo object from the specified kmp.inf file.
 * This code is adapted from legacy method keyboardInfoFromInfFile that loaded data from inf file to NSDictionary.
 *
 * @param path    the full path to the kmp.inf file
 * @return      the corresponding PackageInfo value object or nil if the file cannot be found or parsed
 */
- (KMPackageInfo *) loadPackageInfoFromInfFile:(NSString *)path {
  os_log_debug([KMLogs dataLog], "Loading inf package info from path: %{public}@", path);

  NSMutableArray *files = [NSMutableArray arrayWithCapacity:0];
  NSMutableArray *keyboardInfoArray = [NSMutableArray arrayWithCapacity:0];
  NSMutableArray *fontArray = [NSMutableArray arrayWithCapacity:0];
  KMPackageInfoBuilder *builder = [[KMPackageInfoBuilder alloc] init];
  
  @try {
    NSString *fileContents = [[NSString stringWithContentsOfFile:path encoding:NSWindowsCP1252StringEncoding error:NULL] stringByReplacingOccurrencesOfString:@"\r" withString:@""];
    NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
    ContentType contentType = ctUnknown;
    for (NSString *line in lines) {
      if (!line.length)
        continue;
      
      if ([[line lowercaseString] hasPrefix:[kPackage lowercaseString]]) {
        contentType = ctPackage;
        continue;
      }
      else if ([[line lowercaseString] hasPrefix:[kButtons lowercaseString]]) {
        contentType = ctButtons;
        continue;
      }
      else if ([[line lowercaseString] hasPrefix:[kStartMenu lowercaseString]]) {
        contentType = ctStartMenu;
        continue;
      }
      else if ([[line lowercaseString] hasPrefix:[kStartMenuEntries lowercaseString]]) {
        contentType = ctStartMenuEntries;
        continue;
      }
      else if ([[line lowercaseString] hasPrefix:[kInfo lowercaseString]]) {
        contentType = ctInfo;
        continue;
      }
      else if ([[line lowercaseString] hasPrefix:[kFiles lowercaseString]]) {
        contentType = ctFiles;
        continue;
      }
      
      switch (contentType) {
        case ctPackage: {
          if ([[line lowercaseString] hasPrefix:[kReadMeFile lowercaseString]])
            builder.readmeFilename = [line substringFromIndex:kReadMeFile.length+1];
          else if ([[line lowercaseString] hasPrefix:[kGraphicFile lowercaseString]])
            builder.graphicFilename = [line substringFromIndex:kGraphicFile.length+1];
          break;
        }
        case ctButtons:
          break;
        case ctStartMenu:
          break;
        case ctStartMenuEntries:
          break;
        case ctInfo: {
          if ([[line lowercaseString] hasPrefix:[kName lowercaseString]]) {
            NSString *s = [line substringFromIndex:kName.length+1];
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            builder.packageName = v1;
          }
          else if ([[line lowercaseString] hasPrefix:[kVersion lowercaseString]]) {
            NSString *s = [line substringFromIndex:kVersion.length+1];
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            builder.packageVersion = v1;
          }
          else if ([[line lowercaseString] hasPrefix:[kAuthor lowercaseString]]) {
            NSString *s = [line substringFromIndex:kAuthor.length+1];
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            builder.authorName = v1;
            builder.authorUrl = v2;
          }
          else if ([[line lowercaseString] hasPrefix:[kCopyright lowercaseString]]) {
            NSString *s = [line substringFromIndex:kCopyright.length+1];
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            builder.copyright = v1;
          }
          else if ([[line lowercaseString] hasPrefix:[kWebSite lowercaseString]]) {
            NSString *s = [line substringFromIndex:kWebSite.length+1];
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            builder.website = v1;
          }
          
          break;
        }
        case ctFiles: {
          NSUInteger x = [line rangeOfString:@"="].location;
          if (x == NSNotFound)
            continue;
          
          NSString *s = [line substringFromIndex:x+2];
          if ([[s lowercaseString] hasPrefix:[kFile lowercaseString]]) {
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kFile.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *fileName = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            [files addObject:fileName];
          }
          else if ([[s lowercaseString] hasPrefix:[kFont lowercaseString]]) {
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *fontName = [[[vs objectAtIndex:0] substringFromIndex:kFont.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *fontFileName = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            [fontArray addObject:fontName];
          }
          else if ([[s lowercaseString] hasPrefix:[kKeyboard lowercaseString]]) {
            NSArray *vs = [s componentsSeparatedByString:@"\","];
            NSString *keyboardName = [[[vs objectAtIndex:0] substringFromIndex:kKeyboard.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            NSString *keyboardFileName = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            
            KMKeyboardInfoBuilder *builder = [[KMKeyboardInfoBuilder alloc] init];
            builder.name = keyboardName;
            KMKeyboardInfo *keyboardInfo = [[KMKeyboardInfo alloc] initWithBuilder:builder];
            [keyboardInfoArray addObject:keyboardInfo];
          }
          
          break;
        }
        default:
          break;
      }
    }
  }
  @catch (NSException *e) {
    os_log_error([KMLogs dataLog], "Error = %{public}@", e.description);
    return nil;
  }
  
  builder.keyboards = keyboardInfoArray;
  builder.fonts = fontArray;
  KMPackageInfo *packageInfo = [[KMPackageInfo alloc] initWithBuilder:builder];
  return packageInfo;
}

@end
