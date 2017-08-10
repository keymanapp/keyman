//
//  TimerTarget.h
//  KeymanEngine4MacIM
//
//  Created by Serkan Kurt on 27/02/2015.
//  Copyright (c) 2015 Tavultesoft Pty Ltd. All rights reserved.
//
//  NSTimer retains the target which creates a retain cycle, use this class as NSTimer target and set its target property to the real target to avoid this issue.

#import <Foundation/Foundation.h>

@interface TimerTarget : NSObject
@property (weak, nonatomic) id target;
@end
