//
//  NKey.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface NKey : NSObject

@property (assign, nonatomic) Byte flags;
@property (assign, nonatomic) WORD shift;
@property (assign, nonatomic) WORD vkey;
@property (strong, nonatomic) NSString *text;
@property (strong, nonatomic) NSImage *bitmap;

@end
