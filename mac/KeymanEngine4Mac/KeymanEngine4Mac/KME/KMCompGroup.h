//
//  KMCompGroup.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMCompGroup_h
#define KMCompGroup_h

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import "KMBinaryFileFormat.h"

@interface KMCompGroup : NSObject

@property (strong, nonatomic) NSString *name;
@property (strong, nonatomic) NSMutableArray *keys;
@property (strong, nonatomic) NSString *match;
@property (strong, nonatomic) NSString *noMatch;
@property (assign, nonatomic) BOOL fUsingKeys;

@end

#endif /* KMCompGroup_h */
