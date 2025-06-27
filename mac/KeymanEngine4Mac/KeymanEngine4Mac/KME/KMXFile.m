//
//  KMXFile.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 24/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMXFile.h"
#import "KMCompStore.h"
#import "KMELogs.h"

NSString *const kKMKeyboardNameKey = @"KMKeyboardNameKey";
NSString *const kKMKeyboardVersionKey = @"KMKeyboardVersionKey";
NSString *const kKMKeyboardCopyrightKey = @"KMKeyboardCopyrightKey";
NSString *const kKMKeyboardIconKey = @"KMKeyboardIconKey";
NSString *const kKMVisualKeyboardKey = @"KMVisualKeyboardKey";

@implementation KMXFile

- (id)initWithFilePath:(NSString *)path {
  os_log_info([KMELogs configLog], "initWithFilePath, path: %{public}@", path);
  self = [super init];
  if (self) {
    if (path == nil) {
      _filePath = nil;
      return nil;
    }
    
    NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:path];
    if (file == nil) {
      os_log_error([KMELogs configLog], "Failed to open kmx file");
      _filePath = nil;
      return nil;
    }
    else {
      _filePath = [NSString stringWithString:path];
    }
    
    struct COMP_KEYBOARD cmp_kb;
    [file seekToFileOffset:0];
    size_t size = sizeof(cmp_kb);
    NSData *dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&cmp_kb length:size];
    
    if (cmp_kb.dwFileVersion < VERSION_MIN || cmp_kb.dwFileVersion > VERSION_MAX) {
      [file closeFile];
      return nil;
    }
    
    _identifier = cmp_kb.dwIdentifier;
    _fileVersion = cmp_kb.dwFileVersion;
    _keyboardID = cmp_kb.KeyboardID;
    _isRegistered = (cmp_kb.IsRegistered == 0?NO:YES);
    _version = cmp_kb.version;
 
    // stores not used at init time
    //NSArray *systemStores = [self readSystemStores:cmp_kb file: file];

    [file seekToFileOffset:cmp_kb.dpBitmapOffset];
    NSData *bitmapData = [file readDataOfLength:cmp_kb.dwBitmapSize];
    NSBitmapImageRep *imageRep = [NSBitmapImageRep imageRepWithData:bitmapData];
    _bitmap = [[NSImage alloc] initWithCGImage:imageRep.CGImage size:NSMakeSize(CGImageGetWidth(imageRep.CGImage), CGImageGetHeight(imageRep.CGImage))];
    
    [file closeFile];
  }
  
  return self;
}

/**
 * Read the system stores from the kmx file into an array.
 * Skips over all the normal stores as Keyman for Mac does not process keys but only needs the
 * meta data contained in system stores.
 */
- (NSArray *) readSystemStores:(struct COMP_KEYBOARD)cmp_kb file: (NSFileHandle *) file {
  struct COMP_STORE cmp_str[cmp_kb.cxStoreArray];
  [file seekToFileOffset:cmp_kb.dpStoreArray];
  size_t size = sizeof(cmp_str);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:cmp_str length:size];
  
  NSMutableArray *systemStoreArray = [[NSMutableArray alloc] initWithCapacity:cmp_kb.cxStoreArray];
  
  os_log_info([KMELogs configLog], "COMP_STORE, number of entries: %u", cmp_kb.cxStoreArray);
  for (int i = 0; i < cmp_kb.cxStoreArray; i++) {
    KMCompStore *kmStore = [[KMCompStore alloc] init];
    kmStore.dwSystemID = cmp_str[i].dwSystemID;
    
    /* Keyman for Mac is not processing keys, so no need to save normal stores.
     If dwSystemID is not zero, then this is a system store and may contain
     useful metadata */
    
    if (kmStore.dwSystemID != 0) {
      kmStore.name = [KMXFile UTF16StringWithPointer:cmp_str[i].dpName inFile:file];
      
      if (kmStore.dwSystemID == TSS_VERSION)
        kmStore.string = [[KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file] stringByReplacingOccurrencesOfString:@"\n" withString:@""];
      else {
        kmStore.string = [KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file];
        if (kmStore.dwSystemID == TSS_MNEMONIC && [kmStore.string isEqualToString:@"1"])
          _isMnemonic = YES;
      }
      
      if (kmStore.dwSystemID == TSS_VERSION)
        kmStore.string = [[KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file] stringByReplacingOccurrencesOfString:@"\n" withString:@""];
      else
        kmStore.string = [KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file];
      
      [systemStoreArray addObject:kmStore];
      os_log_info([KMELogs configLog], "loaded store, systemId: %u name: %{public}@ string: %{public}@", kmStore.dwSystemID, kmStore.name, kmStore.string);
    }
  }

  return systemStoreArray;
}

- (NSString *)description {
  NSString *format = @"<%@: %p, File version: 0x%X>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.fileVersion];
  return str;
}

+ (NSDictionary *)keyboardInfoFromKmxFile:(NSString *)path {
  os_log_info([KMELogs configLog], "keyboardInfoFromKmxFile, path: %{public}@", path);
  NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:path];
  os_log_debug([KMELogs configLog], "keyboardInfoFromKmxFile, path: %{public}@", path);

  if (file == nil) {
    os_log_error([KMELogs configLog], "Failed to open file");
    return nil;
  }
  
  
  struct COMP_KEYBOARD cmp_kb;
  [file seekToFileOffset:0];
  size_t size = sizeof(cmp_kb);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&cmp_kb length:size];
  
  if (cmp_kb.dwFileVersion < VERSION_MIN || cmp_kb.dwFileVersion > VERSION_MAX) {
    [file closeFile];
    return nil;
  }
  
  struct COMP_STORE cmp_str[cmp_kb.cxStoreArray];
  [file seekToFileOffset:cmp_kb.dpStoreArray];
  size = sizeof(cmp_str);
  dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:cmp_str length:size];
  
  NSString *nameStr = nil;
  NSString *verStr = nil;
  NSString *copyrightStr = nil;
  NSString *visualKeyboard = nil;
  for (int i = 0; i < cmp_kb.cxStoreArray; i++) {
    if (cmp_str[i].dwSystemID == TSS_NAME)
      nameStr = [KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file];
    if (cmp_str[i].dwSystemID == TSS_VERSION)
      verStr = [[KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file] stringByReplacingOccurrencesOfString:@"\n" withString:@""];
    if (cmp_str[i].dwSystemID == TSS_COPYRIGHT)
      copyrightStr = [KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file];
    if (cmp_str[i].dwSystemID == TSS_VISUALKEYBOARD)
      visualKeyboard = [KMXFile UTF16StringWithPointer:cmp_str[i].dpString inFile:file];
  }
  
  [file seekToFileOffset:cmp_kb.dpBitmapOffset];
  NSData *bitmapData = [file readDataOfLength:cmp_kb.dwBitmapSize];
  NSBitmapImageRep *imageRep = [NSBitmapImageRep imageRepWithData:bitmapData];
  NSImage *icon = [[NSImage alloc] initWithCGImage:imageRep.CGImage size:NSMakeSize(CGImageGetWidth(imageRep.CGImage), CGImageGetHeight(imageRep.CGImage))];
  
  [file closeFile];
  
  if (!nameStr || !nameStr.length) {
    nameStr = [path.lastPathComponent stringByReplacingOccurrencesOfString:@".kmx" withString:@""];
  }
  if (!verStr)
    verStr = @"";
  if (!copyrightStr)
    copyrightStr = @"";
  
  NSDictionary *info;
  if (icon) {
    info = [[NSDictionary alloc] initWithObjectsAndKeys:nameStr, kKMKeyboardNameKey,
            verStr, kKMKeyboardVersionKey,
            copyrightStr, kKMKeyboardCopyrightKey,
            icon, kKMKeyboardIconKey,
            visualKeyboard, kKMVisualKeyboardKey, nil];
  }
  else {
    info = [[NSDictionary alloc] initWithObjectsAndKeys:nameStr, kKMKeyboardNameKey,
            verStr, kKMKeyboardVersionKey,
            copyrightStr, kKMKeyboardCopyrightKey,
            visualKeyboard, kKMVisualKeyboardKey, nil];
  }
  
  return info;
}

+ (NSString *)UTF16StringWithPointer:(DWORD)dp inFile:(NSFileHandle *)file {
  if (dp == 0)
    return nil;
  
  [file seekToFileOffset:dp];
  UTF16Char ch = '\0';
  size_t size = sizeof(ch);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&ch length:size];
  NSMutableString *mStr = [NSMutableString stringWithString:@""];
  while (ch != '\0') {
    //if (ch != '\n')
    [mStr appendString:[NSString stringWithCharacters:&ch length:1]];
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&ch length:size];
  }
  
  return mStr;
}


@end
