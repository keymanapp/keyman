//
//  KMCompStore.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMCompStore.h"

@implementation KMCompStore

- (id)init {
  self = [super init];
  if (self) {
    _systemID = @"";
    _name = @"";
    _string = @"";
  }
  
  return self;
}

- (id)copyWithZone:(NSZone *)zone {
  KMCompStore *newStore = [[[self class] allocWithZone:zone] init];
  
  if (newStore) {
    [newStore setDwSystemID:[self dwSystemID]];
    if ([self name]) newStore.name = [[NSString alloc] initWithString:[self name]];
    if ([self string]) newStore.string = [[NSString alloc] initWithString:[self string]];
  }
  
  return newStore;
}

- (NSString *)description {
  NSString *format = @"<%@:%p ID:0x%X DN:%@ N:%@ S:%@>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.dwSystemID, self.systemID, self.name, self.string];
  return str;
}

- (void)setDwSystemID:(DWORD)dwID {
  _dwSystemID = dwID;
  _systemID = [self TSS_DEF_NAME:_dwSystemID];
}

- (NSString *)TSS_DEF_NAME:(DWORD)d {
  switch (d) {
    case TSS_NONE:
      return @"TSS_NONE";
      break;
    case TSS_BITMAP:
      return @"TSS_BITMAP";
      break;
    case TSS_COPYRIGHT:
      return @"TSS_COPYRIGHT";
      break;
    case TSS_HOTKEY:
      return @"TSS_HOTKEY";
      break;
    case TSS_LANGUAGE:
      return @"TSS_LANGUAGE";
      break;
    case TSS_LAYOUT:
      return @"TSS_LAYOUT";
      break;
    case TSS_MESSAGE:
      return @"TSS_MESSAGE";
      break;
    case TSS_NAME:
      return @"TSS_NAME";
      break;
    case TSS_VERSION:
      return @"TSS_VERSION";
      break;
    case TSS_CAPSONONLY:
      return @"TSS_CAPSONONLY";
      break;
    case TSS_CAPSALWAYSOFF:
      return @"TSS_CAPSALWAYSOFF";
      break;
    case TSS_SHIFTFREESCAPS:
      return @"TSS_SHIFTFREESCAPS";
      break;
    case TSS_LANGUAGENAME:
      return @"TSS_LANGUAGENAME";
      break;
    case TSS_CALLDEFINITION:
      return @"TSS_CALLDEFINITION";
      break;
    case TSS_CALLDEFINITION_LOADFAILED:
      return @"TSS_CALLDEFINITION_LOADFAILED";
      break;
    case TSS_ETHNOLOGUECODE:
      return @"TSS_ETHNOLOGUECODE";
      break;
    case TSS_DEBUG_LINE:
      return @"TSS_DEBUG_LINE";
      break;
    case TSS_MNEMONIC:
      return @"TSS_MNEMONIC";
      break;
    case TSS_INCLUDECODES:
      return @"TSS_INCLUDECODES";
      break;
    case TSS_OLDCHARPOSMATCHING:
      return @"TSS_OLDCHARPOSMATCHING";
      break;
    case TSS_COMPILEDVERSION:
      return @"TSS_COMPILEDVERSION";
      break;
    case TSS_KEYMANCOPYRIGHT:
      return @"TSS_KEYMANCOPYRIGHT:";
      break;
    case TSS_CUSTOMKEYMANEDITION:
      return @"TSS_CUSTOMKEYMANEDITION";
      break;
    case TSS_CUSTOMKEYMANEDITIONNAME:
      return @"TSS_CUSTOMKEYMANEDITIONNAME";
      break;
    case TSS_VISUALKEYBOARD:
      return @"TSS_VISUALKEYBOARD";
      break;
    case TSS_KMW_RTL:
      return @"TSS_KMW_RTL";
      break;
    case TSS_KMW_HELPFILE:
      return @"TSS_HELPFILE";
      break;
    case TSS_KMW_HELPTEXT:
      return @"TSS_HELPTEXT";
      break;
    case TSS_KMW_EMBEDJS:
      return @"TSS_EMBEDJS";
      break;
    case TSS_WINDOWSLANGUAGES:
      return @"TSS_WINDOWSLANGUAGES";
      break;
    case TSS_COMPARISON:
      return @"TSS_COMPARISON";
      break;
    case TSS_PLATFORM:
      return @"TSS_PLATFORM";
      break;
    case TSS_BASELAYOUT:
      return @"TSS_BASELAYOUT";
      break;
    case TSS_LAYER:
      return @"TSS_LAYER";
      break;
    case TSS_PLATFORM_NOMATCH:
      return @"TSS_PLATFORM_NOMATCH";
      break;
    case TSS_PLATFORM_MATCH:
      return @"TSS_PLATFORM_MATCH";
      break;
    case TSS_VKDICTIONARY:
      return @"TSS_VKDICTIONARY";
      break;
    case TSS_LAYOUTFILE:
      return @"TSS_LAYOUTFILE";
      break;
    case TSS_KEYBOARDVERSION:
      return @"TSS_KEYBOARDVERSION";
      break;
    case TSS_KMW_EMBEDCSS:
      return @"TSS_KMW_EMBEDCSS";
      break;
    case TSS_TARGETS:
      return @"TSS_TARGETS";
      break;
    default:
      return nil;
      break;
  }
}

@end
