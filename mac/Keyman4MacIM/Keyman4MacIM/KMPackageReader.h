/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMPackageReader.m
 * Keyman
 *
 * Created by Shawn Schantz on 2021/12/06.
 *
 * Read package information from kmp.json, if it exists. If not, read from kmp.inf.
 * Then create and return KMPackageInfo object.
 *
 */

#import "KMPackageInfo.h"
#import "KMKeyboardInfo.h"
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMPackageReader : NSObject

- (instancetype)init;
- (KMPackageInfo *)loadPackageInfo:(NSString *)path;

@end

NS_ASSUME_NONNULL_END
