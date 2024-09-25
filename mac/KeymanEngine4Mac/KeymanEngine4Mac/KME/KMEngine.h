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
#import "CoreKeyOutput.h"

//#define USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE

@interface KMEngine : NSObject

@property (weak, nonatomic) KMXFile *kmx;

- (id)initWithKMX:(KMXFile *)kmx context:(NSString *)ctxBuf;
- (NSString *)getCoreContextDebug;
- (void)clearCoreContext;
- (void)setCoreContextIfNeeded:(NSString *)context;

- (void)setCoreOptions:(NSString *)key withValue:(NSString *)value;
- (CoreKeyOutput *)processEvent:(NSEvent *)event;

@end

#endif /* KMEngine_h */
