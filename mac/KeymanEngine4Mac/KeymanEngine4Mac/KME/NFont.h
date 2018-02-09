//
//  NFont.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef NFont_h
#define NFont_h

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface NFont : NSObject

@property (strong, nonatomic) NSString *name;
@property (assign, nonatomic) WORD size;
@property (assign, nonatomic) WORD color;

@end

#endif /* NFont_h */
