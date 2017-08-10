//
//  NSArray+Action.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 31/12/2014.
//  Copyright (c) 2015 Tavultesoft Pty Ltd. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface NSMutableArray (Action)

- (void)addAction:(NSDictionary *)action;
- (NSMutableArray *)optimise;

@end