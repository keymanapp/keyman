//
//  NFont.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2015 Tavultesoft Pty Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface NFont : NSObject

@property (strong, nonatomic) NSString *name;
@property (assign, nonatomic) WORD size;
@property (assign, nonatomic) WORD color;

@end
