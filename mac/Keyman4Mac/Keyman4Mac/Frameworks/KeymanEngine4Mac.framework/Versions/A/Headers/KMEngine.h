//
//  KMEngine.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/12/2014.
//  Copyright (c) 2015 Tavultesoft Pty Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMXFile.h"

extern NSString *const Q_STR;
extern NSString *const Q_BACK;
extern NSString *const Q_DEADKEY;
extern NSString *const Q_NUL;
extern NSString *const Q_BEEP;
extern NSString *const Q_RETURN;

extern DWORD VKMap[0x80];

@interface KMEngine : NSObject

@property (weak, nonatomic) KMXFile *kmx;
@property (assign, nonatomic) BOOL debugMode;

- (id)initWithKMX:(KMXFile *)kmx contextBuffer:(NSString *)ctxBuf;
- (void)setContextBuffer:(NSString *)ctxBuf;
- (NSString *)contextBuffer;
- (NSArray *)processEvent:(NSEvent *)event;

@end
