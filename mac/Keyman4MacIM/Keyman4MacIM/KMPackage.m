//
//  KMPackage.m
//  Keyman
//
//  Created by tom on 2/9/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMPackage.h"


@implementation KMPackage

const NSInteger kUnexpectedFileAsscociationType = 42;

+ (BOOL)autosavesInPlace {
    return NO;
}

//- (NSString *)windowNibName {
//    // Override returning the nib file name of the document
//    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
//
//    return @"KMDownloadKBWindowController";
//}


//- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError {
//    // Insert code here to write your document to data of the specified type. If outError != NULL, ensure that you create and set an appropriate error when returning nil.
//    // You can also choose to override -fileWrapperOfType:error:, -writeToURL:ofType:error:, or -writeToURL:ofType:forSaveOperation:originalContentsURL:error: instead.
//    [NSException raise:@"UnimplementedMethod" format:@"%@ is unimplemented", NSStringFromSelector(_cmd)];
//    return nil;
//}

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (NSString *)pathForTemporaryFile
{
    NSString *  result;
    CFUUIDRef   uuid;
    CFStringRef uuidStr;
    
    uuid = CFUUIDCreate(NULL);
    assert(uuid != NULL);
    
    uuidStr = CFUUIDCreateString(NULL, uuid);
    assert(uuidStr != NULL);
    
    result = [NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"%@-%@", @"kmp", uuidStr]];
    assert(result != nil);
    
    CFRelease(uuidStr);
    CFRelease(uuid);
    
    return result;
}

-(BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError * _Nullable __autoreleasing *)outError {

    if (![typeName isEqualToString: @"Keyman Package"]) {
        if (outError != NULL) {
            NSString *description = [@"Unexpected file association for type " stringByAppendingString:typeName];
            NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : description };
            *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
        }
        return NO;
    }

    NSString *tempFile = [self pathForTemporaryFile];
    if (![data writeToFile:tempFile options:NSDataWritingAtomic error:outError])
        return NO;

    BOOL didUnzip = [self.AppDelegate unzipFile:tempFile];
    
    if (!didUnzip && outError != NULL) {
        NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : @"Failed to unzip Keyman Package" };
        *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
    }
    [[NSFileManager defaultManager] removeItemAtPath:tempFile error:nil];
    return didUnzip;
}
@end
