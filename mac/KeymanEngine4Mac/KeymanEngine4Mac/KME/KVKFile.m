//
//  KVKFile.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 5/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KVKFile.h"
#import "NKey.h"
#import "KMELogs.h"

@implementation KVKFile

- (id)initWithFilePath:(NSString *)path {
  self = [super init];
  if (self) {
    if (path == nil) {
      _filePath = nil;
      return nil;
    }
    
    NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:path];
    if (file == nil) {
      os_log_error([KMELogs configLog], "Failed to open kvk file");
      _filePath = nil;
      return nil;
    }
    else {
      _filePath = [NSString stringWithString:path];
    }
    
    [file seekToFileOffset:0];
    DWORD dwMagic;
    size_t size = sizeof(dwMagic);
    NSData *dataBuffer = [file readDataOfLength:size];
    _magic = [[NSString alloc] initWithData:dataBuffer encoding:NSUTF8StringEncoding];
    
    size = sizeof(_version);
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&_version length:size];
    
    size = sizeof(_flags);
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&_flags length:size];
    
    _associatedKeyboard = [KVKFile NStringFromFile:file];
    _ansiFont = [KVKFile NFontFromFile:file];
    _unicodeFont = [KVKFile NFontFromFile:file];
    
    DWORD keyCount;
    size = sizeof(keyCount);
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&keyCount length:size];
  
    _containsAltKeys = NO;
    _containsLeftAltKeys = NO;
    _containsRightAltKeys = NO;

    NSMutableArray *mKeys = [[NSMutableArray alloc] initWithCapacity:keyCount];
    for (int i = 0; i < keyCount; i++) {
      NKey *key = [KVKFile NKeyFromFile:file];
      if(key.modifierFlags & KVKS_ALT) {
        _containsAltKeys = YES;
      }
      if(key.modifierFlags & KVKS_LALT) {
        _containsLeftAltKeys = YES;
      }
      if(key.modifierFlags & KVKS_RALT) {
        _containsRightAltKeys = YES;
      }
      [mKeys addObject:key];
    }

    os_log_debug([KMELogs oskLog], "KVKFile initWithFilePath, containsAltKeys:%d containsLeftAltKeys:%d containsRightAltKeys:%d", _containsAltKeys, _containsLeftAltKeys, _containsRightAltKeys);
    
    _keys = [NSArray arrayWithArray:mKeys];
    

    [file closeFile];
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@:%p\nM:%@\nV:0x%X\nF:0x%X\nAK:%@\nAF:%@\nUF:%@\n>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.magic, self.version, self.flags, self.associatedKeyboard, self.ansiFont, self.unicodeFont];
  return str;
}

+ (NSString *)NStringFromFile:(NSFileHandle *)file {
  WORD strLen;
  size_t size = sizeof(strLen);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&strLen length:size];
  
  WORD ch;
  size = sizeof(ch);
  NSMutableString *mStr = [NSMutableString stringWithString:@""];
  for (int i = 0; i < strLen; i++) {
    dataBuffer = [file readDataOfLength:size];
    [dataBuffer getBytes:&ch length:size];
    if (ch > 0)
      [mStr appendString:[NSString stringWithFormat:@"%C", ch]];
  }
  
  return [NSString stringWithString:mStr];
}

+ (NFont *)NFontFromFile:(NSFileHandle *)file {
  NFont *nfont = [[NFont alloc] init];
  nfont.name = [KVKFile NStringFromFile:file];
  
  DWORD fSize;
  size_t size = sizeof(fSize);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&fSize length:size];
  nfont.size = fSize;
  
  DWORD fColor;
  size = sizeof(fColor);
  dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&fColor length:size];
  nfont.color = fColor;
  
  return nfont;
}

+ (NKey *)NKeyFromFile:(NSFileHandle *)file {
  NKey *nkey = [[NKey alloc] init];
  
  Byte typeFlags;
  size_t size = sizeof(typeFlags);
  NSData *dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&typeFlags length:size];
  nkey.typeFlags = typeFlags;
  
  WORD modifierFlags;
  size = sizeof(modifierFlags);
  dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&modifierFlags length:size];
  nkey.modifierFlags = modifierFlags;
  
  WORD keyCode;
  size = sizeof(keyCode);
  dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&keyCode length:size];
  nkey.keyCode = keyCode;
  
  nkey.text = [KVKFile NStringFromFile:file];
  
  DWORD bmpSize;
  size = sizeof(bmpSize);
  dataBuffer = [file readDataOfLength:size];
  [dataBuffer getBytes:&bmpSize length:size];
  if (bmpSize > 0) {
    dataBuffer = [file readDataOfLength:bmpSize];
    NSBitmapImageRep *imageRep = [NSBitmapImageRep imageRepWithData:dataBuffer];
    const CGFloat colorMasking[6] = {255.0, 255.0, 255.0, 255.0, 255.0, 255.0};
    CGImageRef imageRef = CGImageCreateWithMaskingColors(imageRep.CGImage, colorMasking);
    nkey.bitmap = [[NSImage alloc] initWithCGImage:imageRef size:NSMakeSize(CGImageGetWidth(imageRep.CGImage), CGImageGetHeight(imageRep.CGImage))];
    CGImageRelease(imageRef);
  }
 
  os_log_debug([KMELogs oskLog], "KVKFile NKeyFromFile: %{public}@", nkey);

  return nkey;
}

@end
