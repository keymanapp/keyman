//
//  AppDelegate.m
//  Keyman4Mac
//
//  Created by Serkan Kurt on 1/10/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "AppDelegate.h"
#import "OSKView.h"
#import "KMEngine.h"
#import "KMXFile.h"
#import "KVKFile.h"
#import "MacVKCodes.h"
#import "KMCompStore.h"
#import "KMCompGroup.h"
#import "WindowsVKCodes.h"
#import "NSString+XString.h"

//#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

static BOOL debugMode = YES;

BOOL isKeyMapEnabled;
const unsigned short keyMapSize = 0x80;
unsigned short keyMap[0x80][4];
//unsigned short VKMap[0x80];
//pid_t processID = 0;

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
    //[self setVKMapping];
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
        // Key down event
        NSEvent *mEvent = [NSEvent eventWithCGEvent:event];
        KMEngine *kme = [[KMEngine alloc] initWithKMX:kmx contextBuffer:contextBuffer];
        NSArray *actions = [kme processEvent:mEvent];
        //if (debugMode)
            NSLog(@"%@", actions);
        for (NSDictionary *action in actions) {
            NSString *actionType = [[action allKeys] objectAtIndex:0];
            if ([actionType isEqualToString:Q_STR]) {
                NSString *output = [action objectForKey:actionType];
                UniChar *outCStr = (UniChar *)[output cStringUsingEncoding:NSUTF16StringEncoding];
                unsigned short kc = [mEvent keyCode];
                CGEventRef kEventDown = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)kc, true);
                CGEventRef kEventUp = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)kc, false);
                CGEventKeyboardSetUnicodeString(kEventDown, output.length, outCStr);
                CGEventKeyboardSetUnicodeString(kEventUp, output.length, outCStr);
                CGEventTapPostEvent(proxy, kEventDown);
                CGEventTapPostEvent(proxy, kEventUp);
                [contextBuffer appendString:output];
                CFRelease(kEventDown);
                CFRelease(kEventUp);
            }
            else if ([actionType isEqualToString:Q_BACK]) {
                NSInteger n = [[action objectForKey:actionType] integerValue];
                NSUInteger dk = [contextBuffer deleteLastDeadkeys];
                n -= dk;
                //NSUInteger dc = [contextBuffer deadCharCount];
                //n -= dc;
                
                for (int i = 0; i < n; i++) {
                    CGEventRef kEventDown = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)51, true);
                    CGEventRef kEventUp = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)51, false);
                    CGEventTapPostEvent(proxy, kEventDown);
                    CGEventTapPostEvent(proxy, kEventUp);
                    CFRelease(kEventDown);
                    CFRelease(kEventUp);
                }
                
                [contextBuffer deleteLastNChars:n/*+dc*/];
            }
            else if ([actionType isEqualToString:Q_DEADKEY]) {
                NSUInteger x = [[action objectForKey:actionType] unsignedIntegerValue];
                [contextBuffer appendDeadkey:x];
            }
            else if ([actionType isEqualToString:Q_NUL]) {
                continue;
            }
            else if ([actionType isEqualToString:Q_RETURN]) {
                return NULL;
            }
            else if ([actionType isEqualToString:Q_BEEP]) {
                [[NSSound soundNamed:@"Tink"] play];
            }
            
            handled = YES;
        }
        
        // Apply context changes if not handled
        if (!handled) {
            mEvent = [NSEvent eventWithCGEvent:event];
            unsigned short keyCode = [mEvent keyCode];
            if (keyCode <= 0x33) { // Main keys
                if (keyCode == 0x24) // Enter
                    [contextBuffer appendString:@"\n"];
                else if (keyCode == 0x33) { // Backspace
                    [contextBuffer deleteLastDeadkeys];
                    [contextBuffer deleteLastNChars:1];
                    [contextBuffer deleteLastDeadkeys];
                }
                else
                    [contextBuffer appendString:[mEvent characters]];
            }
            else {
                unichar ch = [[mEvent characters] characterAtIndex:0];
                if (ch >= 0x2A && ch <= 0x39) // Numpad char range
                    [contextBuffer appendString:[mEvent characters]];
                else if (keyCode == 0x4C) // Enter (Numpad)
                    [contextBuffer appendString:@"\n"];
                else if (keyCode >= 0x7B && keyCode <= 0x7E) // Arrow keys
                    contextBuffer = [NSMutableString stringWithString:@""]; // Clear context
                else if (keyCode == 0x73 || keyCode == 0x77 || keyCode == 0x74 || keyCode == 0x79) {
                    // Home, End, Page Up, Page Down
                    contextBuffer = [NSMutableString stringWithString:@""]; // Clear context
                }
                else {
                    // Other keys
                }
            }
        }
    }
    else {
        // Mouse button up event (left | right)
        contextBuffer = [NSMutableString stringWithString:@""]; // Clear context
        if (debugMode)
            NSLog(@"Mouse event");
    }
    
    if (debugMode)
        NSLog(@"contextBuffer = %@\n***", [contextBuffer codeString]);
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
            [alert setAlertStyle:NSWarningAlertStyle];
            [alert runModal];
        }
        
        NSString *packagePath = [kmxPath stringByDeletingLastPathComponent];
        NSDictionary *infoDict = [KMXFile infoDictionaryFromFilePath:kmxPath];
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
        for (KMCompStore *kmStore in kmx.store) {
            NSLog(@"%d: %@[%@]", index, kmStore.systemID, kmStore.string);
            index++;
        }
        
        for (KMCompGroup *gp in kmx.group) {
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
        NSDictionary *infoDict = [KMXFile infoDictionaryFromFilePath:path];
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
/*
// Creates a VK map to convert Mac VK codes to Windows VK codes
- (void)setVKMapping {
    VKMap[MVK_A] = VK_KEY_A;                        // A
    VKMap[MVK_S] = VK_KEY_S;                        // S
    VKMap[MVK_D] = VK_KEY_D;                        // D
    VKMap[MVK_F] = VK_KEY_F;                        // F
    VKMap[MVK_H] = VK_KEY_H;                        // H
    VKMap[MVK_G] = VK_KEY_G;                        // G
    VKMap[MVK_Z] = VK_KEY_Z;                        // Z
    VKMap[MVK_X] = VK_KEY_X;                        // X
    VKMap[MVK_C] = VK_KEY_C;                        // C
    VKMap[MVK_V] = VK_KEY_V;                        // V
    //    0x0A  = nil
    VKMap[MVK_B] = VK_KEY_B;                        // B
    VKMap[MVK_Q] = VK_KEY_Q;                        // Q
    VKMap[MVK_W] = VK_KEY_W;                        // W
    VKMap[MVK_E] = VK_KEY_E;                        // E
    VKMap[MVK_R] = VK_KEY_R;                        // R
    VKMap[MVK_Y] = VK_KEY_Y;                        // Y
    VKMap[MVK_T] = VK_KEY_T;                        // T
    VKMap[MVK_1] = VK_KEY_1;                        // 1
    VKMap[MVK_2] = VK_KEY_2;                        // 2
    VKMap[MVK_3] = VK_KEY_3;                        // 3
    VKMap[MVK_4] = VK_KEY_4;                        // 4
    VKMap[MVK_6] = VK_KEY_6;                        // 6
    VKMap[MVK_5] = VK_KEY_5;                        // 5
    VKMap[MVK_EQUAL] = VK_EQUAL;                    // =
    VKMap[MVK_9] = VK_KEY_9;                        // 9
    VKMap[MVK_7] = VK_KEY_7;                        // 7
    VKMap[MVK_MINUS] = VK_MINUS;                    // -
    VKMap[MVK_8] = VK_KEY_8;                        // 8
    VKMap[MVK_0] = VK_KEY_0;                        // 0
    VKMap[MVK_RIGHT_BRACKET] = VK_RIGHT_BRACKET;    // ]
    VKMap[MVK_O] = VK_KEY_O;                        // O
    VKMap[MVK_U] = VK_KEY_U;                        // U
    VKMap[MVK_LEFT_BRACKET] = VK_LEFT_BRACKET;      // [
    VKMap[MVK_I] = VK_KEY_I;                        // I
    VKMap[MVK_P] = VK_KEY_P;                        // P
    VKMap[MVK_ENTER] = VK_ENTER;                    // <Enter>
    VKMap[MVK_L] = VK_KEY_L;                        // L
    VKMap[MVK_J] = VK_KEY_J;                        // J
    VKMap[MVK_QUOTE] = VK_QUOTE;                    // '
    VKMap[MVK_K] = VK_KEY_K;                        // K
    VKMap[MVK_SEMICOLON] = VK_SEMICOLON;            // ;
    VKMap[MVK_BACKSLASH] = VK_BACKSLASH;            // '\'
    VKMap[MVK_COMMA] = VK_COMMA;                    // ,
    VKMap[MVK_SLASH] = VK_SLASH;                    // '/'
    VKMap[MVK_N] = VK_KEY_N;                        // N
    VKMap[MVK_M] = VK_KEY_M;                        // M
    VKMap[MVK_PERIOD] = VK_PERIOD;                  // .
    VKMap[MVK_TAB] = VK_TAB;                        // <Tab>
    VKMap[MVK_SPACE] = VK_SPACE;                    // <Space>
    VKMap[MVK_GRAVE] = VK_GRAVE;                    // `
    VKMap[MVK_BACKSPACE] = VK_BACKSPACE;            // <Backspace>
    
    // Num Pad
    VKMap[0x41] = VK_NUMPAD_DECIMAL;                // Decimal .
    VKMap[0x43] = VK_NUMPAD_MULTIPLY;               // Multiply *
    VKMap[0x45] = VK_NUMPAD_ADD;                    // Add +
    VKMap[0x47] = 0x00;                             // <Clear>      *** Undefined ***
    VKMap[0x4B] = VK_NUMPAD_DIVIDE;                 // Divide /
    VKMap[0x4C] = 0x00;                             // <Enter>      *** Undefined ***
    VKMap[0x4E] = VK_NUMPAD_SUBTRACT;               // Subtract -
    VKMap[0x51] = 0x00;                             // Equal =      *** Undefined ***
    VKMap[0x52] = VK_NUMPAD0;                       // 0
    VKMap[0x53] = VK_NUMPAD1;                       // 1
    VKMap[0x54] = VK_NUMPAD2;                       // 2
    VKMap[0x55] = VK_NUMPAD3;                       // 3
    VKMap[0x56] = VK_NUMPAD4;                       // 4
    VKMap[0x57] = VK_NUMPAD5;                       // 5
    VKMap[0x58] = VK_NUMPAD6;                       // 6
    VKMap[0x59] = VK_NUMPAD7;                       // 7
    VKMap[0x5B] = VK_NUMPAD8;                       // 8
    VKMap[0x5C] = VK_NUMPAD9;                       // 9
}*/

// *** No longer used ***
- (void)clearkeyMap {
    for (int i = 0; i < keyMapSize; i++) {
        keyMap[i][0] = 0x0000; // Default state
        keyMap[i][1] = 0x0000; // Shift state
        keyMap[i][2] = 0x0000; // Alpha-shift state (Caps Lock on)
        keyMap[i][3] = 0x0000; // Alternate state
    }
}

// *** No longer used ***
- (void)setTurkishQMapping {
    keyMap[12][0] = 0x0071; // q
    keyMap[12][1] = 0x0051; // Q
    keyMap[12][2] = 0x0051; // Q
    keyMap[12][3] = 0x0040; // @
    
    keyMap[14][0] = 0x0065; // e
    keyMap[14][1] = 0x0045; // E
    keyMap[14][2] = 0x0045; // E
    keyMap[14][3] = 0x20AC; // €
    
    keyMap[18][0] = 0x0031; // 1
    keyMap[18][1] = 0x0021; // !
    keyMap[18][2] = 0x0000; //
    keyMap[18][3] = 0x003E; // >
    
    keyMap[19][0] = 0x0032; // 2
    keyMap[19][1] = 0x0027; // '
    keyMap[19][2] = 0x0000; //
    keyMap[19][3] = 0x00A3; // £
    
    keyMap[20][0] = 0x0033; // 3
    keyMap[20][1] = 0x005E; // ^
    keyMap[20][2] = 0x0000; //
    keyMap[20][3] = 0x0023; // #
    
    keyMap[21][0] = 0x0034; // 4
    keyMap[21][1] = 0x002B; // +
    keyMap[21][2] = 0x0000; //
    keyMap[21][3] = 0x0024; // $
    
    keyMap[22][0] = 0x0036; // 6
    keyMap[22][1] = 0x0026; // &
    keyMap[22][2] = 0x0000; //
    keyMap[22][3] = 0x0000; //
    
    keyMap[23][0] = 0x0035; // 5
    keyMap[23][1] = 0x0025; // %
    keyMap[23][2] = 0x0000; //
    keyMap[23][3] = 0x00BD; // ½
    
    keyMap[24][0] = 0x002D; // -
    keyMap[24][1] = 0x005F; // _
    keyMap[24][2] = 0x0000; //
    keyMap[24][3] = 0x007C; // |
    
    keyMap[25][0] = 0x0039; // 9
    keyMap[25][1] = 0x0029; // )
    keyMap[25][2] = 0x0000; //
    keyMap[25][3] = 0x005D; // ]
    
    keyMap[26][0] = 0x0037; // 7
    keyMap[26][1] = 0x002F; // /
    keyMap[26][2] = 0x0000; //
    keyMap[26][3] = 0x007B; // {
    
    keyMap[27][0] = 0x002A; // *
    keyMap[27][1] = 0x003F; // ?
    keyMap[27][2] = 0x0000; //
    keyMap[27][3] = 0x005C; // \
    
    keyMap[28][0] = 0x0038; // 8
    keyMap[28][1] = 0x0028; // (
    keyMap[28][2] = 0x0000; //
    keyMap[28][3] = 0x005B; // [
    
    keyMap[29][0] = 0x0030; // 0
    keyMap[29][1] = 0x003D; // =
    keyMap[29][2] = 0x0000; //
    keyMap[29][3] = 0x007D; // }
    
    keyMap[30][0] = 0x00FC; // ü
    keyMap[30][1] = 0x00DC; // Ü
    keyMap[30][2] = 0x00DC; // Ü
    keyMap[30][3] = 0x007E; // ~
    
    keyMap[33][0] = 0x011F; // ğ
    keyMap[33][1] = 0x011E; // Ğ
    keyMap[33][2] = 0x011E; // Ğ
    keyMap[33][3] = 0x00A8; // ¨
    
    keyMap[34][0] = 0x0131; // ı
    keyMap[34][1] = 0x0049; // I
    keyMap[34][2] = 0x0049; // I
    keyMap[34][3] = 0x0000; //
    
    keyMap[39][0] = 0x0069; // i
    keyMap[39][1] = 0x0130; // İ
    keyMap[39][2] = 0x0130; // İ
    keyMap[39][3] = 0x0000; //
    
    keyMap[41][0] = 0x015F; // ş
    keyMap[41][1] = 0x015E; // Ş
    keyMap[41][2] = 0x015E; // Ş
    keyMap[41][3] = 0x00B4; // ´
    
    keyMap[42][0] = 0x002C; // ,
    keyMap[42][1] = 0x003B; // ;
    keyMap[42][2] = 0x0000; //
    keyMap[42][3] = 0x0060; // `
    
    keyMap[43][0] = 0x00F6; // ö
    keyMap[43][1] = 0x00D6; // Ö
    keyMap[43][2] = 0x00D6; // Ö
    keyMap[43][3] = 0x0000; //
    
    keyMap[44][0] = 0x002E; // .
    keyMap[44][1] = 0x003A; // :
    keyMap[44][2] = 0x0000; //
    keyMap[44][3] = 0x0000; //
    
    keyMap[47][0] = 0x00E7; // ç
    keyMap[47][1] = 0x00C7; // Ç
    keyMap[47][2] = 0x00C7; // Ç
    keyMap[47][3] = 0x0000; //
    
    keyMap[50][0] = 0x0022; // "
    keyMap[50][1] = 0x00E9; // é
    keyMap[50][2] = 0x0000; //
    keyMap[50][3] = 0x003C; // <
}

@end
