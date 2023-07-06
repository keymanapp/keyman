//
//  KMEngine.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMEngine_h
#define KMEngine_h

#import <Foundation/Foundation.h>
#import "KMXFile.h"

//#define USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE

extern NSString *const Q_STR;
extern NSString *const Q_BACK;
extern NSString *const Q_DEADKEY;
extern NSString *const Q_NUL;
extern NSString *const Q_BEEP;
extern NSString *const Q_RETURN;
extern NSString *const Q_SAVEOPT;

extern DWORD VKMap[0x80];

@interface KMEngine : NSObject

@property (weak, nonatomic) KMXFile *kmx;
@property (assign, nonatomic) BOOL debugMode;

- (id)initWithKMX:(KMXFile *)kmx contextBuffer:(NSString *)ctxBuf;
- (void)setCoreContext:(NSString *)context;
//TODO: remove additional context methods
- (void)setContextBuffer:(NSString *)ctxBuf;
- (NSString *)contextBuffer;
- (void)setStore:(DWORD)storeID withValue:(NSString *)value;
- (NSArray *)processEvent:(NSEvent *)event;
// TODO: remove test method
- (NSArray *)experimentallyProcessEventForUnitTestingOnly:(NSEvent *)event usingCore:(BOOL)useCore;
- (void)setUseVerboseLogging:(BOOL)useVerboseLogging;

@end

#endif /* KMEngine_h */
