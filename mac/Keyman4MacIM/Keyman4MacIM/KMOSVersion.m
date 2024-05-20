//
//  KMOSVersion.m
//  Keyman
//
//  Created by Randy Boring on 2/13/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

#import "KMOSVersion.h"

static CFStringRef sSystemVersionPath = CFSTR("/System/Library/CoreServices/SystemVersion.plist");

@implementation KMOSVersion
+(u_int16_t)SystemVersion
{
  NSDictionary *systemVersionDict = [NSDictionary dictionaryWithContentsOfFile:(__bridge NSString*)sSystemVersionPath];
  if (NULL ==systemVersionDict)
  {
    NSLog(@"could not read system version dictionary file: %@", sSystemVersionPath);
    return 0;
  }
  NSString *systemVersionString = [systemVersionDict objectForKey:@"ProductVersion"];
  if (NULL ==systemVersionString)
  {
    NSLog(@"could not retrieve system version from dictionary at %@ with %d keys", sSystemVersionPath, (int)systemVersionDict.count);
    return 0;
  }
  NSArray *systemVersionStringParts = [systemVersionString componentsSeparatedByString:@"."];
  if (systemVersionStringParts.count < 2)
  {
    NSLog(@"Too few parts to the system version (probably): %@", systemVersionString);
    return [systemVersionString intValue]; // hopefully this will at least be the major OS version, e.g., 10 (all versions so far!)
  }
  
  NSString *majVersionStr = systemVersionStringParts[0];
  u_int16_t systemVersion = majVersionStr.intValue << 8;
  NSString *minVersionStr = systemVersionStringParts[1];
  systemVersion |= minVersionStr.intValue << 4;
  if (systemVersionStringParts.count > 2)
  {
    NSString *bugVersionStr = systemVersionStringParts[2];
    systemVersion |= bugVersionStr.intValue;
  }
  if (systemVersionStringParts.count > 3)
  {
    NSLog(@"Portions of System Version string after third part unused: %@", systemVersionString);
  }
  return systemVersion;
}

+(u_int16_t)Version_10_13_1
{
  return 0x0AD1; // 10.13.1 in hex by nibbles, the way the old Gestalt call used to do it
}

+(u_int16_t)Version_10_13_3
{
  return 0x0AD3; // 10.13.3 in hex by nibbles, the way the old Gestalt call used to do it
}

@end
