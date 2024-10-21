//
//  NKey.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef NKey_h
#define NKey_h

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface NKey : NSObject

@property (assign, nonatomic) Byte typeFlags;
@property (assign, nonatomic) WORD modifierFlags;
@property (assign, nonatomic) WORD vkey;
@property (strong, nonatomic) NSString *text;
@property (strong, nonatomic) NSImage *bitmap;

@end

#endif /* NKey_h */
