//
//  KVKFile.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 5/12/2014.
//  Copyright (c) 2015 Tavultesoft Pty Ltd. All rights reserved.
//

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

- (id)initWithFilePath:(NSString *)path;

@end
