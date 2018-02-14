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

- (void)makeWindowControllers {
    if ([self.AppDelegate debugMode])
        NSLog(@"Attempting to display configuration window...");
    
    [[self.AppDelegate configWindow] showWindow:nil];
}

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (NSString *)pathForTemporaryKmpFile:(NSString *)kmpFilename
{
    NSString *  result = [NSTemporaryDirectory() stringByAppendingPathComponent:kmpFilename];
    assert(result != nil);
    return result;
}

-(BOOL)readFromFileWrapper:(NSFileWrapper *)fileWrapper ofType:(NSString *)typeName error:(NSError * _Nullable __autoreleasing *)outError {
    
    NSString *filename = [fileWrapper filename];
    
    if ([self.AppDelegate debugMode])
        NSLog(@"readFromFileWrapper called with file: %@", filename);
    
    if (![typeName isEqualToString: @"Keyman Package"]) {
        if (outError != NULL) {
            NSString *description = [@"Unexpected file association for type " stringByAppendingString:typeName];
            NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : description,
                                               NSFilePathErrorKey : filename
                                               };
            *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
        }
        return NO;
    }
    
    if (![self userConfirmsInstallationOfPackageFile:filename]) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Keyman Package file installation cancelled by user.");
        return YES; // Returning NO causes the system to report a failure to the user.
    }
    
    NSString *tempFile = [self pathForTemporaryKmpFile:filename];
    NSURL *fileURL = [[NSURL alloc] initFileURLWithPath:tempFile];
    if (![fileWrapper writeToURL:fileURL options:NSFileWrapperWritingAtomic originalContentsURL:nil error:outError])
        return NO;
    
    BOOL didUnzip = [self.AppDelegate unzipFile:tempFile];
    
    if (!didUnzip && outError != NULL) {
        NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : @"Failed to unzip Keyman Package" };
        *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
    }
    [[NSFileManager defaultManager] removeItemAtPath:tempFile error:nil];
    return didUnzip;
}

-(BOOL)userConfirmsInstallationOfPackageFile:(NSString *)fileName {
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:NSLocalizedString(@"Yes", @"Alert button")];
    [alert addButtonWithTitle:NSLocalizedString(@"No", @"Alert button")];
    [alert setMessageText:NSLocalizedString(@"Install Keyman Package?", @"Alert message text when user double-clicks a KMP file.")];
    NSString *info = NSLocalizedString(@"Do you want the Keyman Input Method to install this Package?\rFile: %@", @"Alert informative text when user double-clicks a KMP file. Parameter is the name of the KMP file.");
    info = [NSString localizedStringWithFormat:info, fileName];
    [alert setInformativeText:info];
    [alert setAlertStyle:NSInformationalAlertStyle];
    
    if ([self.AppDelegate debugMode])
        NSLog(@"Asking user to confirm installation...");
    
    BOOL result = [alert runModal] == NSAlertFirstButtonReturn;
    return result;
}

// This tells the system that we aren't reading the Keyman Package file lazily.
-(BOOL) isEntireFileLoaded {
    return YES;
}

//-(BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError * _Nullable __autoreleasing *)outError {
//
//    if (![typeName isEqualToString: @"Keyman Package"]) {
//        if (outError != NULL) {
//            NSString *description = [@"Unexpected file association for type " stringByAppendingString:typeName];
//            NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : description };
//            *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
//        }
//        return NO;
//    }
//
//    NSString *tempFile = [self pathForTemporaryFile];
//    if (![data writeToFile:tempFile options:NSDataWritingAtomic error:outError])
//        return NO;
//
//    BOOL didUnzip = [self.AppDelegate unzipFile:tempFile];
//
//    if (!didUnzip && outError != NULL) {
//        NSDictionary *errorDictionary = @{ NSLocalizedDescriptionKey : @"Failed to unzip Keyman Package" };
//        *outError = [[NSError alloc] initWithDomain:@"Keyman" code:kUnexpectedFileAsscociationType userInfo:errorDictionary];
//    }
//    [[NSFileManager defaultManager] removeItemAtPath:tempFile error:nil];
//    return didUnzip;
//}
@end
