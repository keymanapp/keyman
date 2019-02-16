//
//  KMOSVersion.h
//  Keyman
//
//  Created by Randy Boring on 2/13/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMOSVersion : NSObject

+(u_int16_t)SystemVersion;
+(u_int16_t)Version_10_13_1; // 0x0AD1 but don't depend on this
+(u_int16_t)Version_10_13_3; // 0x0AD3

@end

NS_ASSUME_NONNULL_END
