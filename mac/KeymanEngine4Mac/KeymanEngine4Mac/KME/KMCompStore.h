//
//  KMCompStore.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMCompStore_h
#define KMCompStore_h

#import <Foundation/Foundation.h>
#import "KMBinaryFileFormat.h"

@interface KMCompStore : NSObject

@property (assign, nonatomic) DWORD dwSystemID;
@property (strong, nonatomic, readonly) NSString *systemID;
@property (strong, nonatomic) NSString *name;
@property (strong, nonatomic) NSString *string;

- (void)setDwSystemID:(DWORD)dwID;
- (id)copyWithZone:(NSZone *)zone;

@end

#endif /* KMCompStore_h */
