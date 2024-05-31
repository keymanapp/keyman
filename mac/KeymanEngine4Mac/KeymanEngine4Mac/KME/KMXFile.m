//
//  KMXFile.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 24/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMXFile.h"
#import "KMCompStore.h"
#import "KMCompGroup.h"
#import "KMCompKey.h"
#import "NSString+XString.h"
#import "KMELogs.h"

NSString *const kKMKeyboardNameKey = @"KMKeyboardNameKey";
NSString *const kKMKeyboardVersionKey = @"KMKeyboardVersionKey";
NSString *const kKMKeyboardCopyrightKey = @"KMKeyboardCopyrightKey";
NSString *const kKMKeyboardIconKey = @"KMKeyboardIconKey";
NSString *const kKMVisualKeyboardKey = @"KMVisualKeyboardKey";

@implementation KMXFile

- (id)initWithFilePath:(NSString *)path {
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
    NSMutableArray *mStartGroup = [[NSMutableArray alloc] initWithCapacity:2];
    [mStartGroup addObject:[NSNumber numberWithInt:cmp_kb.StartGroup[0]]];
    [mStartGroup addObject:[NSNumber numberWithInt:cmp_kb.StartGroup[1]]];
    _startGroup = [[NSArray alloc] initWithArray:mStartGroup];
    _flags = cmp_kb.dwFlags;
    _hotKey = cmp_kb.dwHotKey;
    
    struct COMP_STORE cmp_str[cmp_kb.cxStoreArray];
    [file seekToFileOffset:cmp_kb.dpStoreArray];
    size = sizeof(cmp_str);
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:cmp_str length:size];
    NSMutableArray *mStore = [[NSMutableArray alloc] initWithCapacity:cmp_kb.cxStoreArray];
    for (int i = 0; i < cmp_kb.cxStoreArray; i++) {
      KMCompStore *kmStore = [[KMCompStore alloc] init];
      kmStore.dwSystemID = cmp_str[i].dwSystemID;
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
      
      [mStore addObject:kmStore];
    }
    
    _store = [[NSArray alloc] initWithArray:mStore];
    _storeSaved = [[NSArray alloc] initWithArray:mStore copyItems:YES];
    
    struct COMP_GROUP cmp_grp[cmp_kb.cxGroupArray];
    [file seekToFileOffset:cmp_kb.dpGroupArray];
    size = sizeof(cmp_grp);
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:cmp_grp length:size];
    
    NSMutableArray *mGroup = [[NSMutableArray alloc] initWithCapacity:cmp_kb.cxGroupArray];
    for (int i = 0; i < cmp_kb.cxGroupArray; i++) {
      KMCompGroup *kmGrp = [[KMCompGroup alloc] init];
      kmGrp.name = [KMXFile UTF16StringWithPointer:cmp_grp[i].dpName inFile:file];
      kmGrp.match = [KMXFile UTF16StringWithPointer:cmp_grp[i].dpMatch inFile:file];
      kmGrp.noMatch = [KMXFile UTF16StringWithPointer:cmp_grp[i].dpNoMatch inFile:file];
      kmGrp.fUsingKeys = cmp_grp[i].fUsingKeys;
      
      struct COMP_KEY cmp_keys[cmp_grp[i].cxKeyArray];
      [file seekToFileOffset:cmp_grp[i].dpKeyArray];
      size = sizeof(cmp_keys);
      dataBuffer = [file readDataOfLength:size];
      [dataBuffer getBytes:cmp_keys length:size];
      
      for (int j = 0; j < cmp_grp[i].cxKeyArray; j++) {
        KMCompKey *kmKey = [[KMCompKey alloc] init];
        kmKey.key = cmp_keys[j].Key;
        kmKey.line = cmp_keys[j].Line;
        kmKey.shiftFlags = cmp_keys[j].ShiftFlags;
        kmKey.output = [KMXFile UTF16StringWithPointer:cmp_keys[j].dpOutput inFile:file];
        kmKey.context = [KMXFile UTF16StringWithPointer:cmp_keys[j].dpContext inFile:file];
        [kmGrp.keys addObject:kmKey];
      }
      
      [mGroup addObject:kmGrp];
    }
    _group = [[NSArray alloc] initWithArray:mGroup];
    
    [file seekToFileOffset:cmp_kb.dpBitmapOffset];
    NSData *bitmapData = [file readDataOfLength:cmp_kb.dwBitmapSize];
    NSBitmapImageRep *imageRep = [NSBitmapImageRep imageRepWithData:bitmapData];
    _bitmap = [[NSImage alloc] initWithCGImage:imageRep.CGImage size:NSMakeSize(CGImageGetWidth(imageRep.CGImage), CGImageGetHeight(imageRep.CGImage))];
    
    [file closeFile];
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@: %p, File version: 0x%X Flags: 0x%X Store: %@ Group: %@>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.fileVersion, self.flags, self.store, self.group];
  return str;
}

- (BOOL)isValid {
  for (KMCompGroup *gp in self.group) {
    if ((gp.match && ![gp.match isValidCode]) || (gp.noMatch && ![gp.noMatch isValidCode]))
      return NO;
    
    for (KMCompKey *kmKey in gp.keys) {
      if (![kmKey.context isValidCode] || ![kmKey.output isValidCode])
        return NO;
    }
  }
  
  return YES;
}

+ (NSDictionary *)keyboardInfoFromKmxFile:(NSString *)path {
  NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:path];
  
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
