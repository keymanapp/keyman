//
//  TimerTarget.m
//  KeymanEngine4MacIM
//
//  Created by Serkan Kurt on 27/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "TimerTarget.h"

@implementation TimerTarget

- (void)timerAction:(NSTimer *)timer {
  [self.target performSelector:@selector(timerAction:) withObject:timer];
}

@end
