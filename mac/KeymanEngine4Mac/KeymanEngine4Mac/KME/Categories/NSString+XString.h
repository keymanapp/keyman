//
//  NSString+XString.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 8/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KMBinaryFileFormat.h"

@interface NSString (XString)

- (NSUInteger)xLength;
- (NSUInteger)xLengthIncludingIf;
- (NSUInteger)deadKeyCount;
- (NSString *)lastNChars:(NSUInteger)n;
- (NSString *)codeString;
- (BOOL)isValidCode;
+ (NSString *)nullChar;

@end

@interface NSMutableString (XString)

- (void)appendDeadkey:(NSUInteger)index;
- (void)appendNullChar;
- (NSUInteger)deleteLastDeadkeys;
- (NSUInteger)deleteLastNullChars;
- (NSUInteger)removeAllNullChars;
- (void)deleteLastNChars:(NSUInteger)n;

@end
