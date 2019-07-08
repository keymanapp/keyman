//
//  NSArray+Action.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 31/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSArray+Action.h"
#import "KMEngine.h"

@implementation NSMutableArray (Action)

- (void)addAction:(NSDictionary *)newAction {
    if (!self.count) {
        [self addObject:newAction];
    }
    else {
        NSDictionary *preAction = [self objectAtIndex:self.count-1];
        NSString *preActionType = [[preAction allKeys] objectAtIndex:0];
        NSString *newActionType = [[newAction allKeys] objectAtIndex:0];
        if ([preActionType isEqualToString:newActionType]) {
            if ([preActionType isEqualToString:Q_STR]) {
                NSString *preStr = [preAction objectForKey:preActionType];
                NSString *newStr = [newAction objectForKey:newActionType];
                NSString *outStr = [NSString stringWithFormat:@"%@%@", preStr, newStr];
                NSDictionary *mAction = [[NSDictionary alloc] initWithObjectsAndKeys:outStr, Q_STR, nil];
                [self removeLastObject];
                [self addObject:mAction];
            }
            else if ([preActionType isEqualToString:Q_BACK]) {
                NSUInteger preNum = [[preAction objectForKey:preActionType] unsignedIntegerValue];
                NSUInteger newNum = [[newAction objectForKey:newActionType] unsignedIntegerValue];
                NSNumber *outNum = [NSNumber numberWithUnsignedInteger:preNum+newNum];
                NSDictionary *mAction = [[NSDictionary alloc] initWithObjectsAndKeys:outNum, Q_BACK, nil];
                [self removeLastObject];
                [self addObject:mAction];
            }
            else {
                [self addObject:newAction];
            }
        }
        else if ([preActionType isEqualToString:Q_STR] && [newActionType isEqualToString:Q_BACK]) {
            NSString *preStr = [preAction objectForKey:preActionType];
            NSUInteger backNum = [[newAction objectForKey:newActionType] unsignedIntegerValue];
            if (preStr.length > backNum) {
                NSString *newStr = [preStr substringToIndex:preStr.length - backNum];
                NSDictionary *mAction = [[NSDictionary alloc] initWithObjectsAndKeys:newStr, Q_STR, nil];
                [self removeLastObject];
                [self addObject:mAction];
            }
            else if (preStr.length < backNum) {
                NSUInteger newNum = backNum - preStr.length;
                NSNumber *outNum = [NSNumber numberWithUnsignedInteger:newNum];
                NSDictionary *mAction = [[NSDictionary alloc] initWithObjectsAndKeys:outNum, Q_BACK, nil];
                [self removeLastObject];
                [self addObject:mAction];
            }
            else {
                [self removeLastObject];
            }
        }
        else {
            [self addObject:newAction];
        }
    }
}

- (NSMutableArray *)optimise {
    if (self.count >= 2) {
        NSDictionary *action;
        NSDictionary *preAction;
        NSDictionary *newAction;
        NSString *actionType;
        NSString *preActionType;
        NSString *str;
        NSString *preStr;
        NSString *newStr;
        NSUInteger intVal;
        NSUInteger preIntVal;
        NSNumber *newIntVal;

        int lastIndex = (int)self.count - 1;
        for (int i = lastIndex; i >= 1; i--) {
            action = [self objectAtIndex:i];
            actionType = [[action allKeys] objectAtIndex:0];
            preAction = [self objectAtIndex:i-1];
            preActionType = [[preAction allKeys] objectAtIndex:0];
            if ([actionType isEqualToString:preActionType]) {
                if ([actionType isEqualToString:Q_STR]) {
                    str = [action objectForKey:actionType];
                    preStr = [action objectForKey:preActionType];
                    newStr = [NSString stringWithFormat:@"%@%@", preStr, str];
                    newAction = [[NSDictionary alloc] initWithObjectsAndKeys:newStr, Q_STR, nil];
                    [self replaceObjectAtIndex:i-1 withObject:newAction];
                    [self removeObjectAtIndex:i];
                }
                else if ([actionType isEqualToString:Q_BACK]) {
                    intVal = [[action objectForKey:actionType] unsignedIntegerValue];
                    preIntVal = [[preAction objectForKey:preActionType] unsignedIntegerValue];
                    newIntVal = [NSNumber numberWithUnsignedInteger:preIntVal+intVal];
                    newAction = [[NSDictionary alloc] initWithObjectsAndKeys:newIntVal, Q_BACK, nil];
                    [self replaceObjectAtIndex:i-1 withObject:newAction];
                    [self removeObjectAtIndex:i];
                }
            }
        }
    }
    
    return self;
}

@end
