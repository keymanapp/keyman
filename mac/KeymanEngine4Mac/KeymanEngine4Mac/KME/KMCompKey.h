//
//  KMCompKey.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface KMCompKey : NSObject

@property (assign, nonatomic) WORD key;
@property (assign, nonatomic) DWORD line;
@property (assign, nonatomic) DWORD shiftFlags;
@property (strong, nonatomic) NSString *output;
@property (strong, nonatomic) NSString *context;

@end
