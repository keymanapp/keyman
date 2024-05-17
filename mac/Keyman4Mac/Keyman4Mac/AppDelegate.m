//
//  AppDelegate.m
//  Keyman4Mac
//
//  Created by Serkan Kurt on 1/10/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "AppDelegate.h"
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>
#import <os/log.h>

static BOOL debugMode = YES;

BOOL isKeyMapEnabled;
const unsigned short keyMapSize = 0x80;

// Dictionary keys
NSString *const kContextBufferKey = @"ContextBuffer";
NSString *const kKMXFileKey = @"KMXFile";

@interface AppDelegate ()
@property (weak) IBOutlet NSWindow *window;
@property (nonatomic, strong) KVKFile *kvk;
@property (nonatomic, strong) IBOutlet NSComboBox *kbComboBox;
@property (nonatomic, strong) IBOutlet OSKView *oskView;
@property (nonatomic, strong) IBOutlet NSImageView *imgView;
@property (nonatomic, strong) NSMutableArray *kmxList;
@property (nonatomic, strong) NSMutableDictionary *kbData;
@property (assign) CFMachPortRef machPortRef;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    // Insert code here to initialize your application
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidResize:) name:NSWindowDidResizeNotification object:self.window];
    [self setKMXList];
    self.kbData = [[NSMutableDictionary alloc] initWithCapacity:0];
    NSMutableString *contextBuffer = [NSMutableString stringWithString:@""];
    [self.kbData setObject:contextBuffer forKey:kContextBufferKey];
    //self.context = [NSMutableString stringWithString:@""];
    [self.kbComboBox setDelegate:self];
    [self.kbComboBox selectItemAtIndex:0];
    [self enableAssistiveDevices];
    [self createEventTap];
    /*
    NSArray *kvkFiles = [self KVKFiles];
    for (NSString *path in kvkFiles){
        KVKFile *kvkFile = [[KVKFile alloc] initWithFilePath:path];
        if (debugMode)
            NSLog(@"%@", kvkFile);
    }*/
}

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    // Insert code here to tear down your application
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)awakeFromNib {
    // Keep the aspect ratio constant at its current value
    [self.window setAspectRatio:self.window.frame.size];
    NSSize size = self.window.frame.size;
    [self.window setMaxSize:NSMakeSize(size.width*1.6, size.height*1.6)];
    [self.window setMinSize:NSMakeSize(size.width*0.8, size.height*0.8)];
    [self.window setBackgroundColor:[NSColor colorWithRed:241.0/255.0 green:242.0/255.0 blue:242.0/255.0 alpha:1.0]];
}

- (void)windowDidResize:(NSNotification *)notification {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "AppDelegate windowDidResize");
   [self.oskView resizeOSKLayout];
}

- (BOOL)createEventTap {
    CGEventMask keyboardMask = CGEventMaskBit(kCGEventKeyDown);
    //CGEventMask mouseMask = CGEventMaskBit(kCGEventLeftMouseUp)|CGEventMaskBit(kCGEventRightMouseUp);
    CGEventMask mask = keyboardMask; // + mouseMask;
    self.machPortRef = CGEventTapCreate(
                                        kCGAnnotatedSessionEventTap,
                                        kCGHeadInsertEventTap,
                                        kCGEventTapOptionDefault,
                                        mask,
                                        (CGEventTapCallBack)eventTapFunction,
                                        (__bridge void *)(self.kbData));
    if (!self.machPortRef) {
        NSLog(@"Can't install keyboard & mouse hook.");
        return NO;
    }
    
    CFRunLoopSourceRef keyboardEventSrc = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, self.machPortRef, 0);
    if (!keyboardEventSrc)
        return NO;
    
    CFRunLoopRef runLoop = CFRunLoopGetCurrent();
    if (!runLoop) {
        CFRelease(keyboardEventSrc);
        return NO;
    }
    
    CFRunLoopAddSource(runLoop, keyboardEventSrc, kCFRunLoopDefaultMode);
    CFRelease(keyboardEventSrc);
    return YES;
}

CGEventRef eventTapFunction(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *refcon) {
    // If key map is not enable, return the event without modifying
    if (!isKeyMapEnabled)
        return event;
    
    // Get a pointer to the context
    NSMutableDictionary *kbData = (__bridge NSMutableDictionary *)(refcon);
    NSMutableString *contextBuffer = [kbData objectForKey:kContextBufferKey];
    KMXFile *kmx = [kbData objectForKey:kKMXFileKey];
    BOOL handled = NO;
    
    if (type == NX_KEYDOWN) {
        NSLog(@"AppDelegate eventTapFunction key down event: %@", event);
        // Key down event
        NSEvent *mEvent = [NSEvent eventWithCGEvent:event];
        KMEngine *kme = [[KMEngine alloc] initWithKMX:kmx context:contextBuffer verboseLogging:debugMode];
        CoreKeyOutput *coreKeyOutput = [kme processEvent:mEvent];
        if (coreKeyOutput) {
            if (coreKeyOutput.hasTextToInsert) {
                NSString *output = coreKeyOutput.textToInsert;
                UniChar *outCStr = (UniChar *)[output cStringUsingEncoding:NSUTF16StringEncoding];
                unsigned short kc = [mEvent keyCode];
                CGEventRef kEventDown = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)kc, true);
                CGEventRef kEventUp = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)kc, false);
                CGEventKeyboardSetUnicodeString(kEventDown, output.length, outCStr);
                CGEventKeyboardSetUnicodeString(kEventUp, output.length, outCStr);
                CGEventTapPostEvent(proxy, kEventDown);
                CGEventTapPostEvent(proxy, kEventUp);
                CFRelease(kEventDown);
                CFRelease(kEventUp);
            }
            if (coreKeyOutput.emitKeystroke) {
                return NULL;
            }
            if (coreKeyOutput.alert) {
                [[NSSound soundNamed:@"Tink"] play];
            }
            
            handled = YES;
        }
    }
    
    return handled?NULL:event;
}

- (void)enableAssistiveDevices {
    NSDictionary *errorDict;
    NSAppleEventDescriptor *returnDescriptor = NULL;
    
    NSAppleScript *scriptObject = [[NSAppleScript alloc] initWithSource:
                                   @"\
                                   on isUIScriptingOn()\n\
                                   tell application \"System Events\" to set isUIScriptingEnabled to UI elements enabled\n\
                                   return isUIScriptingEnabled\n\
                                   end isUIScriptingOn\n\
                                   on turnUIScriptingOn(switch)\n\
                                   tell application \"System Events\"\n\
                                   activate\n\
                                   set UI elements enabled to switch\n\
                                   end tell\n\
                                   end turnUIScriptingOn\n\
                                   on run\n\
                                   if not isUIScriptingOn() then\n\
                                   display dialog \"Keyman would like to enable access for assistive devices to work correctly\"\n\
                                   turnUIScriptingOn(true)\n\
                                   display dialog \"Access for assistive devices for Keyman is now on\"\n\
                                   end if\n\
                                   end run"];
    
    returnDescriptor = [scriptObject executeAndReturnError: &errorDict];
    
    if (returnDescriptor != NULL) {
        // successful execution
        if (kAENullEvent != [returnDescriptor descriptorType]) {
            // script returned an AppleScript result
            if (cAEList == [returnDescriptor descriptorType]) {
                // result is a list of other descriptors
            }
            else {
                // coerce the result to the appropriate ObjC type
            }
        }
    }
    else {
        // no script result, handle error here
    }
}

- (void)comboBoxSelectionDidChange:(NSNotification *)notification {
    if (self.kbComboBox.indexOfSelectedItem == 0) {
        isKeyMapEnabled = NO;
        self.imgView.image = nil;
        //self.oskView.hidden = YES;
    }
    else {
        isKeyMapEnabled = YES;
        [self clearContext];
        NSString *kmxPath = [self.kmxList objectAtIndex:self.kbComboBox.indexOfSelectedItem-1];
        KMXFile *kmx = [[KMXFile alloc] initWithFilePath:kmxPath];
        if (![kmx isValid]) {
            NSAlert *alert = [[NSAlert alloc] init];
            [alert addButtonWithTitle:@"OK"];
            [alert setMessageText:@"Invalid KMX file"];
            [alert setInformativeText:@"This KMX file contains some invalid code!"];
            [alert setAlertStyle:NSAlertStyleWarning];
            [alert runModal];
        }
        
        NSString *packagePath = [kmxPath stringByDeletingLastPathComponent];
        NSDictionary *infoDict = [KMXFile keyboardInfoFromKmxFile:kmxPath];
        NSString *kvkFilename = [infoDict objectForKey:kKMVisualKeyboardKey];
        if (kvkFilename && [kvkFilename length]) {
            NSString *kvkPath = [packagePath stringByAppendingPathComponent:kvkFilename];
            _kvk = [[KVKFile alloc] initWithFilePath:kvkPath];
            [self.oskView setKvk:_kvk];
            //NSLog(@"kvk:nkeys: \n %@", _kvk.keys);
        }
        
        [self.kbData setObject:kmx forKey:kKMXFileKey];
        self.imgView.image = kmx.bitmap;
        
        int index = 0;
        for (NSObject *kmStore in kmx.store) {
            NSLog(@"%d: %@", index, kmStore);
            index++;
        }
        
        for (NSObject *gp in kmx.group) {
            if (debugMode) {
                NSLog(@"Group %@", gp);
                //NSLog(@"match = %@", gp.match);
                //NSLog(@"nomatch = %@", gp.noMatch);
                /*
                for (KMCompKey *kmKey in gp.keys) {
                    NSLog(@"\nKey: %@", kmKey);
                }*/
            }
        }
    }
}

- (void)postKeyUpDown {
    //
}

- (void)clearContext {
    NSMutableString *contextBuffer = [self.kbData objectForKey:kContextBufferKey];
    [contextBuffer setString:@""];
}

- (void)setKMXList {
    self.kmxList = [NSMutableArray arrayWithArray:[self KMXFiles]];
    NSMutableArray *kmxDesc = [[NSMutableArray alloc] initWithCapacity:0];
    for (NSString *path in self.kmxList) {
        NSDictionary *infoDict = [KMXFile keyboardInfoFromKmxFile:path];
        if (!infoDict)
            continue;
        
        //if (debugMode)
        //    NSLog(@"%@", infoDict);
        //NSString *str = [NSString stringWithFormat:@"%@ (%@)", [infoDict objectForKey:kKMKeyboardNameKey], [infoDict objectForKey:kKMKeyboardVersionKey]];
        NSString *str = [infoDict objectForKey:kKMKeyboardNameKey];
        [kmxDesc addObject:str];
    }
    
    [self.kbComboBox addItemsWithObjectValues:kmxDesc];
}

- (NSString *)keyboardsPath {
    if (_keyboardsPath == nil) {
        NSString *documentDirPath = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        _keyboardsPath = [documentDirPath stringByAppendingPathComponent:@"Keyboards"];
    }
    
    return _keyboardsPath;
}

- (NSArray *)KMXFiles {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:self.keyboardsPath];
    NSMutableArray *kmxFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"kmx"])
            [kmxFiles addObject:[self.keyboardsPath stringByAppendingPathComponent:filePath]];
    }
    
    return kmxFiles;
}

- (NSArray *)KVKFiles {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:self.keyboardsPath];
    NSMutableArray *kvkFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"kvk"])
            [kvkFiles addObject:[self.keyboardsPath stringByAppendingPathComponent:filePath]];
    }
    
    return kvkFiles;
}

@end
