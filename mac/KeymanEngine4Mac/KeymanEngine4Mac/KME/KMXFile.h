//
//  KMXFile.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 24/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMXFile_h
#define KMXFile_h

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import "KMBinaryFileFormat.h"

extern NSString *const kKMKeyboardNameKey;
extern NSString *const kKMKeyboardVersionKey;
extern NSString *const kKMKeyboardCopyrightKey;
extern NSString *const kKMKeyboardIconKey;
extern NSString *const kKMVisualKeyboardKey;

@interface KMXFile : NSObject

@property (assign, nonatomic, readonly) DWORD identifier;
@property (assign, nonatomic, readonly) DWORD fileVersion;
@property (assign, nonatomic, readonly) DWORD keyboardID;
@property (assign, nonatomic, readonly) BOOL isRegistered;
@property (assign, nonatomic, readonly) BOOL isMnemonic;
@property (assign, nonatomic, readonly) DWORD version;
@property (strong, nonatomic, readonly) NSArray *store;
@property (strong, nonatomic, readonly) NSArray *storeSaved;
@property (strong, nonatomic, readonly) NSArray *group;
@property (strong, nonatomic, readonly) NSArray *startGroup;
@property (assign, nonatomic, readonly) DWORD flags;
@property (assign, nonatomic, readonly) DWORD hotKey;
@property (strong, nonatomic, readonly) NSImage *bitmap;
@property (strong, nonatomic, readonly) NSString *filePath;

- (id)initWithFilePath:(NSString *)path;
- (BOOL)isValid;

/*!
* Provides an info dictionary from a KMX file which includes keyboard name, version, copyright information and keyboard icon.
* \param path KMX file path
* \returns NSDictionary with keyboard name, version, copyright & icon.
*/
+ (NSDictionary *)keyboardInfoFromKmxFile:(NSString *)path;

@end

#endif /* KMXFile_h */
