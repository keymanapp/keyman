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
BOOL _installingKmp = NO;
NSString *filename = nil;

+ (BOOL)autosavesInPlace {
    return NO;
}

// This method gets called whenever a user double-clicks a KMP file, regardless of whether
// they decide to install it or cancel the installation. It also gets called one time up-front
// when Keyman first loads.
- (void)makeWindowControllers {
    // The first part of this conditional ensures that the config window is created,
    // even if we are not going to display it. If this doesn't happen, then when the
    // user tries to open the config window manually via the menu, it won't work.
    if ([self.AppDelegate configWindow] != nil && _installingKmp) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Attempting to display configuration window...");
        
        [[self.AppDelegate configWindow] showWindow:nil];
        _installingKmp = NO;
    }
    if (filename != nil) { // Should be true every time except maybe the very first time.
        // Since we aren't really opening this file as a document, after reading it (or deciding not to)
        // we need to close this document so the controller doesn't hold onto it. Otherwise, if the user
        // double-clicks the same KMP file a second time it will just assume we want to open the window
        // to display the already open document, rather than attempting to read it again.
        if ([self.AppDelegate debugMode])
            NSLog(@"Closing document to release presenter...");
        [self close];
        filename = nil;
    }
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
    
    filename = [fileWrapper filename];
    
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
    _installingKmp = YES;
    
    NSString *tempFile = [self pathForTemporaryKmpFile:filename];
    NSURL *fileURL = [[NSURL alloc] initFileURLWithPath:tempFile];
    if (![fileWrapper writeToURL:fileURL options:NSFileWrapperWritingAtomic originalContentsURL:nil error:outError]) {
        return NO;
    }
    
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
    
    // Attempted the following (with and without the first line) to try to
    // get Keyman to be active foreground app so the alert message would not
    // appear behind other window(s). Unfortunately, it doesn't work, probably
    // because of Keyman being an input method.
//    [[self.AppDelegate configWindow] showWindow:nil];
//    [[NSRunningApplication currentApplication] activateWithOptions:0];
    BOOL result = [alert runModal] == NSAlertFirstButtonReturn;
    return result;
}

// This tells the system that we aren't reading the Keyman Package file lazily.
-(BOOL) isEntireFileLoaded {
    return YES;
}
@end
