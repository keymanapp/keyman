//
//  KVKFile.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 5/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KVKFile_h
#define KVKFile_h

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import "KMBinaryFileFormat.h"
#import "NFont.h"

@interface KVKFile : NSObject

@property (strong, nonatomic, readonly) NSString *magic;
@property (assign, nonatomic, readonly) DWORD version;
@property (assign, nonatomic, readonly) Byte flags;
@property (strong, nonatomic, readonly) NSString *associatedKeyboard;
@property (strong, nonatomic, readonly) NFont *ansiFont;
@property (strong, nonatomic, readonly) NFont *unicodeFont;
@property (strong, nonatomic, readonly) NSArray *keys;
@property (strong, nonatomic, readonly) NSString *filePath;
@property (readonly) BOOL containsAltKeys;
@property (readonly) BOOL containsLeftAltKeys;
@property (readonly) BOOL containsRightAltKeys;

- (id)initWithFilePath:(NSString *)path;

@end

#endif /* KVKFile_h */
