//
//  KMPackage.m
//  Keyman
//
//  Created by tom on 2/9/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMPackage.h"
#import "KMConfigurationWindowController.h"
#import "KMLogs.h"

@implementation KMPackage

const NSInteger kUnexpectedFileAsscociationType = 42;
NSString *filenameTempKMP = nil;
NSString *filename = nil; // This is the filename from the FileWrapper (if any)


-(NSString *) getOrigKmpFilename {
  return filename;
}

-(NSString *) getTempKmpFilename {
  return filenameTempKMP;
}

+ (BOOL)autosavesInPlace {
  return NO;
}

// This method gets called whenever a user double-clicks a KMP file, regardless of whether
// they decide to install it or cancel the installation. It also gets called one time up-front
// when Keyman first loads.
- (void)makeWindowControllers {
  os_log_debug([KMLogs dataLog], "KMP - in KMPackage.makeWindowControllers");

  if (filename != nil) { // Should be true every time except maybe the very first time (when loading).
    filename = nil;
    [self closeIfFilesAreReleased];
  }
}

-(void) closeIfFilesAreReleased {
  // Since we aren't really opening this file as a document, after reading it (or deciding not to)
  // we need to close this document so the controller doesn't hold onto it. Otherwise, if the user
  // double-clicks the same KMP file a second time it will just assume we want to open the window
  // to display the already open document, rather than attempting to read it again.

  os_log_debug([KMLogs dataLog], "Closing document to release presenter...");
  if (filename == nil && filenameTempKMP == nil)
    [self close];
}

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(BOOL)readFromFileWrapper:(NSFileWrapper *)fileWrapper ofType:(NSString *)typeName error:(NSError * _Nullable __autoreleasing *)outError {
  
  filename = [fileWrapper filename];
  
  os_log_debug([KMLogs dataLog], "readFromFileWrapper called with file: %{public}@", filename);

  if (![typeName isEqualToString: @"Keyman Package"]) {
    if (outError != NULL) {
      NSString *description = [@"Unexpected file association for type " stringByAppendingString:typeName];
      NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : description,
                                         NSFilePathErrorKey : filename
      };
      *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
    }
    return NO;
  }
  
  // Strip out any browser-appended number, e.g. khmer_angkor (1).kmp
  // We work on a copy of the file, so this is safe to do -- the fileWrapper
  // saves to the filename we give it.
  NSError *error = nil;
  NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@" \\(\\d+\\)\\.kmp$" options:NSRegularExpressionCaseInsensitive error:&error];
  filename = [regex stringByReplacingMatchesInString:filename 
                                             options:0
                                               range:NSMakeRange(0, [filename length])
                                        withTemplate:@".kmp"];
  
  filenameTempKMP = [NSTemporaryDirectory() stringByAppendingPathComponent:filename];
  assert(filenameTempKMP != nil);    
  NSURL *fileURL = [[NSURL alloc] initFileURLWithPath:filenameTempKMP];
  if (![fileWrapper writeToURL:fileURL options:NSFileWrapperWritingAtomic originalContentsURL:nil error:outError]) {
    filenameTempKMP = nil;
    return NO;
  }
  
  KMConfigurationWindowController *configWindowController = (KMConfigurationWindowController *)[self.AppDelegate configWindow];
  [configWindowController handleRequestToInstallPackage:self];
  
  return YES; // Even if the user decides not to, we need to return Yes to prevent displaying an error message.
}

-(void) releaseTempKMPFile {
  assert(filenameTempKMP != nil);
  [[NSFileManager defaultManager] removeItemAtPath:filenameTempKMP error:nil];
  filenameTempKMP = nil;
  [self closeIfFilesAreReleased];
}

// This tells the system that we aren't reading the Keyman Package file lazily.
-(BOOL) isEntireFileLoaded {
  return YES;
}
@end
