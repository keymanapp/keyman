//
//  KMInputMethodAppDelegate.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 12/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

// *** TO INVESTIGATE ***
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (activateServerWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (deactivateServerWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (menusDictionaryWithClientAsync:reply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (modesWithClientAsync:reply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (commitCompositionWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (hidePalettes) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (sessionFinished) block performed very slowly (0.00 secs)

#import "KMInputMethodAppDelegate.h"
#import "KMConfigurationWindowController.h"
#import "KMDownloadKBWindowController.h"
#import "ZipArchive.h"

NSString *const kKMSelectedKeyboardKey = @"KMSelectedKeyboardKey";
NSString *const kKMActiveKeyboardsKey = @"KMActiveKeyboardsKey";
NSString *const kKMAlwaysShowOSKKey = @"KMAlwaysShowOSKKey";
NSString *const kKeymanKeyboardDownloadCompletedNotification = @"kKeymanKeyboardDownloadCompletedNotification";

NSString *const kPackage = @"[Package]";
NSString *const kButtons = @"[Buttons]";
NSString *const kStartMenu = @"[StartMenu]";
NSString *const kStartMenuEntries = @"[StartMenuEntries]";
NSString *const kInfo = @"[Info]";
NSString *const kFiles = @"[Files]";

NSString *const kAuthor = @"Author";
NSString *const kCopyright = @"Copyright";
NSString *const kFile = @"File";
NSString *const kFont = @"Font";
NSString *const kGraphicFile = @"GraphicFile";
NSString *const kKeyboard = @"Keyboard";
NSString *const kName = @"Name";
NSString *const kReadMeFile = @"ReadMeFile";
NSString *const kVersion = @"Version";
NSString *const kWebSite = @"WebSite";
NSString *const kWelcome = @"Welcome";

typedef enum {
    ctPackage, ctButtons, ctStartMenu, ctStartMenuEntries, ctInfo, ctFiles, ctUnknown
} ContentType;

@implementation KMInputMethodAppDelegate
@synthesize kme = _kme;
@synthesize kmx = _kmx;
@synthesize kvk = _kvk;
@synthesize keyboardName = _keyboardName;
@synthesize keyboardsPath = _keyboardsPath;
@synthesize kmxFileList = _kmxFileList;
@synthesize selectedKeyboard = _selectedKeyboard;
@synthesize activeKeyboards = _activeKeyboards;
@synthesize contextBuffer = _contextBuffer;
@synthesize alwaysShowOSK = _alwaysShowOSK;
@synthesize useNullChar = _useNullChar;

- (id)init {
    self = [super init];
    if (self) {
        // _debugMode = YES; // Disable before release
        [[NSAppleEventManager sharedAppleEventManager] setEventHandler:self
                                                           andSelector:@selector(handleURLEvent:withReplyEvent:)
                                                         forEventClass:kInternetEventClass
                                                            andEventID:kAEGetURL];
    }
    
    return self;
}

- (void)handleURLEvent:(NSAppleEventDescriptor*)event withReplyEvent:(NSAppleEventDescriptor*)replyEvent {
    NSMutableString *urlStr = [NSMutableString stringWithString:[[event paramDescriptorForKeyword:keyDirectObject] stringValue]];
    [urlStr replaceOccurrencesOfString:@"keyman:" withString:@"keyman/" options:0 range:NSMakeRange(0, 7)];
    NSURL *url = [NSURL URLWithString:urlStr];
    if (_debugMode)
        NSLog(@"url = %@", url);
    
    if ([url.lastPathComponent isEqualToString:@"download"]) {
        if (_connection != nil)
            return;
        
        NSURL *downloadUrl;
        NSArray *params = [[url query] componentsSeparatedByString:@"&"];
        for (NSString *value in params) {
            NSUInteger index = NSNotFound;
            if ((index = [value rangeOfString:@"filename="].location) != NSNotFound)
                _downloadFilename = [NSString stringWithString:[value substringFromIndex:index+9]];
            else if ((index = [value rangeOfString:@"url="].location) != NSNotFound)
                downloadUrl = [NSURL URLWithString:[[NSString stringWithString:[value substringFromIndex:index+4]] stringByRemovingPercentEncoding]];
        }
        
        if (downloadUrl && _downloadFilename) {
            if (_infoWindow.window != nil)
                [_infoWindow close];
            
            [self.downloadInfoView setInformativeText:self.downloadFilename];
            if (self.configWindow.window != nil) {
                [self.configWindow.window makeKeyAndOrderFront:nil];
                if (![[self.configWindow.window childWindows] containsObject:self.downloadKBWindow.window]) {
                    [self.configWindow.window addChildWindow:self.downloadKBWindow.window ordered:NSWindowAbove];
                }
                [self.downloadKBWindow.window centerInParent];
                [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
                [self.downloadInfoView beginSheetModalForWindow:self.downloadKBWindow.window
                                                  modalDelegate:self
                                                 didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                                    contextInfo:nil];
                [self downloadKeyboardFromURL:downloadUrl];
            }
            else {
                [self.downloadKBWindow.window centerInParent];
                [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
                [self.downloadKBWindow.window setLevel:NSFloatingWindowLevel];
                [self.downloadInfoView beginSheetModalForWindow:self.downloadKBWindow.window
                                                  modalDelegate:self
                                                 didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                                    contextInfo:nil];
                [self downloadKeyboardFromURL:downloadUrl];
            }
        }
    }
}

- (NSMenu *)menu {
    return _menu;
}

- (KMEngine *)kme {
    if (_kme == nil) {
        _kme = [[KMEngine alloc] initWithKMX:nil contextBuffer:self.contextBuffer];
    }
    
    return _kme;
}

- (void)setKmx:(KMXFile *)kmx {
    _kmx = kmx;
    [self.kme setKmx:_kmx];
}

- (void)setKvk:(KVKFile *)kvk {
    _kvk = kvk;
    if (_oskWindow != nil)
        [_oskWindow resetOSK];
}

- (void)setKeyboardName:(NSString *)keyboardName {
    _keyboardName = keyboardName;
    if (_oskWindow != nil)
        [_oskWindow.window setTitle:self.oskWindowTitle];
}

- (NSString *)oskWindowTitle {
    if (_keyboardName == nil || !_keyboardName.length)
        return [NSString stringWithFormat:@"Keyman"];
    else
        return [NSString stringWithFormat:@"%@ - Keyman", _keyboardName];
}

- (void)setAlwaysShowOSK:(BOOL)alwaysShowOSK {
    _alwaysShowOSK = alwaysShowOSK;
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setBool:alwaysShowOSK forKey:kKMAlwaysShowOSKKey];
    [userData synchronize];
}

- (BOOL)alwaysShowOSK {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    _alwaysShowOSK = [userData boolForKey:kKMAlwaysShowOSKKey];
    return _alwaysShowOSK;
}

- (NSString *)keyboardsPath {
    if (_keyboardsPath == nil) {
        NSString *documentDirPath = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        _keyboardsPath = [documentDirPath stringByAppendingPathComponent:@"Keyman-Keyboards"];
        NSFileManager *fm = [NSFileManager defaultManager];
        if (![fm fileExistsAtPath:_keyboardsPath]) {
            [fm createDirectoryAtPath:_keyboardsPath withIntermediateDirectories:YES attributes:nil error:nil];
        }
    }
    
    return _keyboardsPath;
}

- (NSArray *)kmxFileList {
    if (_kmxFileList == nil) {
        NSArray *kmxFiles = [self KMXFiles];
        _kmxFileList = [[NSMutableArray alloc] initWithCapacity:0];
        NSMutableArray *others = nil;
        for (NSString *filePath in kmxFiles) {
            NSString *packageFolder = [self packageFolderFromPath:filePath];
            NSInteger index = [self indexForPackageFolder:packageFolder];
            if ([packageFolder isEqualToString:@"Others"]) {
                if (others == nil)
                    others = [NSMutableArray arrayWithCapacity:0];
                [others addObject:filePath];
            }
            else {
                if (index >= 0) {
                    [[_kmxFileList objectAtIndex:index] addObject:filePath];
                }
                else {
                    NSMutableArray *pArray = [[NSMutableArray alloc] initWithObjects:filePath, nil];
                    [_kmxFileList addObject:pArray];
                }
            }
        }
        
        if (others != nil)
            [_kmxFileList addObject:others];
    }
    
    return _kmxFileList;
}

- (NSString *)kmxFilePathAtIndex:(NSUInteger)index {
    NSUInteger x = 0;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        x++;
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        for (NSString *path in pArray) {
            if (index == x)
                return path;
            x++;
        }
    }
    
    return nil;
}

- (NSString *)packagePathAtIndex:(NSUInteger)index {
    NSString *packagePath = nil;
    NSUInteger x = 0;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        if (!pArray.count) {
            i++;
            x++;
            continue;
        }
        
        if (index >= x && index <= (x+pArray.count)) {
            packagePath = [[pArray objectAtIndex:0] stringByDeletingLastPathComponent];
            break;
        }
        
        x += (pArray.count+1);
    }
    
    return packagePath;
}

- (NSInteger)indexForPackageFolder:(NSString *)packageFolder {
    NSInteger index = -1;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        if (pArray.count > 0) {
            NSString *packageFolder2 = [self packageFolderFromPath:[pArray objectAtIndex:0]];
            if ([packageFolder isEqualToString:packageFolder2]) {
                index = i;
                break;
            }
        }
    }
    
    return index;
}

- (NSString *)packageFolderFromPath:(NSString *)path {
    NSString *packageFolder = nil;
    NSString *sourcePath = [self keyboardsPath];
    NSString *mPath = [NSString stringWithString:[path stringByDeletingLastPathComponent]];
    if ([mPath isEqualToString:sourcePath])
        return @"Others";
    
    while (![mPath isEqualToString:sourcePath]) {
        packageFolder = [mPath lastPathComponent];
        mPath = [mPath stringByDeletingLastPathComponent];
    }
    
    return packageFolder;
}

- (NSString *)packageNameFromFolder:(NSString *)packageFolder {
    NSString *packageName = nil;
    NSString *path = [[self keyboardsPath] stringByAppendingPathComponent:packageFolder];
    NSString *fileContents = [NSString stringWithContentsOfFile:[path stringByAppendingPathComponent:@"kmp.inf"] encoding:NSUTF8StringEncoding error:NULL];
    NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
    BOOL hasInfo = NO;
    for (NSString *line in lines) {
        if ([line startsWith:@"[Info]"]) {
            hasInfo = YES;
            continue;
        }
        
        if (hasInfo && [line startsWith:@"Name="]) {
            NSString *value = [[[line substringFromIndex:5] componentsSeparatedByString:@","] objectAtIndex:0];
            packageName = [NSString stringWithString:[value stringByReplacingOccurrencesOfString:@"\"" withString:@""]];
            break;
        }
    }
    
    if (packageName == nil)
        packageName = packageFolder;
    
    return packageName;
}

- (NSDictionary *)infoDictionaryFromFile:(NSString *)infoFile {
    NSMutableDictionary *infoDict = [NSMutableDictionary dictionaryWithCapacity:0];
    NSMutableArray *files = [NSMutableArray arrayWithCapacity:0];
    NSMutableArray *fonts = [NSMutableArray arrayWithCapacity:0];
    NSMutableArray *kbs = [NSMutableArray arrayWithCapacity:0];
    
    @try {
        NSString *fileContents = [[NSString stringWithContentsOfFile:infoFile encoding:NSUTF8StringEncoding error:NULL] stringByReplacingOccurrencesOfString:@"\r" withString:@""];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        ContentType contentType = ctUnknown;
        for (NSString *line in lines) {
            if (!line.length)
                continue;
            
            if ([line startsWith:kPackage]) {
                contentType = ctPackage;
                continue;
            }
            else if ([line startsWith:kButtons]) {
                contentType = ctButtons;
                continue;
            }
            else if ([line startsWith:kStartMenu]) {
                contentType = ctStartMenu;
                continue;
            }
            else if ([line startsWith:kStartMenuEntries]) {
                contentType = ctStartMenuEntries;
                continue;
            }
            else if ([line startsWith:kInfo]) {
                contentType = ctInfo;
                continue;
            }
            else if ([line startsWith:kFiles]) {
                contentType = ctFiles;
                continue;
            }
            
            switch (contentType) {
                case ctPackage: {
                    if ([line startsWith:kReadMeFile])
                        [infoDict setObject:[line substringFromIndex:kReadMeFile.length+1] forKey:kReadMeFile];
                    else if ([line startsWith:kGraphicFile])
                        [infoDict setObject:[line substringFromIndex:kGraphicFile.length+1] forKey:kGraphicFile];
                    
                    break;
                }
                case ctButtons:
                    break;
                case ctStartMenu:
                    break;
                case ctStartMenuEntries: {
                    if ([line startsWith:kWelcome]) {
                        NSString *s = [line substringFromIndex:kWelcome.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kWelcome];
                    }
                    
                    break;
                }
                case ctInfo: {
                    if ([line startsWith:kName]) {
                        NSString *s = [line substringFromIndex:kName.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kName];
                    }
                    else if ([line startsWith:kVersion]) {
                        NSString *s = [line substringFromIndex:kVersion.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kVersion];
                    }
                    else if ([line startsWith:kAuthor]) {
                        NSString *s = [line substringFromIndex:kAuthor.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kAuthor];
                    }
                    else if ([line startsWith:kCopyright]) {
                        NSString *s = [line substringFromIndex:kCopyright.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kCopyright];
                    }
                    else if ([line startsWith:kWebSite]) {
                        NSString *s = [line substringFromIndex:kWebSite.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kWebSite];
                    }
                    
                    break;
                }
                case ctFiles: {
                    NSUInteger x = [line rangeOfString:@"="].location;
                    if (x == NSNotFound)
                        continue;
                    
                    NSString *s = [line substringFromIndex:x+2];
                    if ([s startsWith:kFile]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kFile.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [files addObject:@[v1, v2]];
                    }
                    else if ([s startsWith:kFont]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kFont.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [fonts addObject:@[v1, v2]];
                    }
                    else if ([s startsWith:kKeyboard]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kKeyboard.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [kbs addObject:@[v1, v2]];
                    }
                    
                    break;
                }
                default:
                    break;
            }
        }
    }
    @catch (NSException *e) {
        NSLog(@"Error = %@", e.description);
        return nil;
    }
    
    if (files.count)
        [infoDict setValue:files forKey:kFile];
    if (fonts.count)
        [infoDict setValue:fonts forKey:kFont];
    if (kbs.count)
        [infoDict setValue:kbs forKey:kKeyboard];
    
    return infoDict.count?[NSDictionary dictionaryWithDictionary:infoDict]:nil;
}

- (NSString *)selectedKeyboard {
    if (_selectedKeyboard == nil) {
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        _selectedKeyboard = [userData objectForKey:kKMSelectedKeyboardKey];
    }
    
    return _selectedKeyboard;
}

- (void)setSelectedKeyboard:(NSString *)selectedKeyboard {
    _selectedKeyboard = selectedKeyboard;
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:_selectedKeyboard forKey:kKMSelectedKeyboardKey];
    [userData synchronize];
}

- (NSMutableArray *)activeKeyboards {
    if (!_activeKeyboards) {
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        _activeKeyboards = [[userData arrayForKey:kKMActiveKeyboardsKey] mutableCopy];
        if (!_activeKeyboards)
            _activeKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
    }
    
    return _activeKeyboards;
}

- (void)saveActiveKeyboards {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:_activeKeyboards forKey:kKMActiveKeyboardsKey];
    [userData synchronize];
    [self resetActiveKeyboards];
    [self setKeyboardsSubMenu];
}

- (void)clearActiveKeyboards {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:nil forKey:kKMActiveKeyboardsKey];
    [userData synchronize];
    [self setKeyboardsSubMenu];
}

- (void)resetActiveKeyboards {
    NSMutableArray *pathsToRemove = [[NSMutableArray alloc] initWithCapacity:0];
    for (NSString *path in self.activeKeyboards) {
        if (![[NSFileManager defaultManager] fileExistsAtPath:path])
            [pathsToRemove addObject:path];
    }
    
    if (pathsToRemove.count > 0) {
        [self.activeKeyboards removeObjectsInArray:pathsToRemove];
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        [userData setObject:_activeKeyboards forKey:kKMActiveKeyboardsKey];
        [userData synchronize];
    }
}

- (NSMutableString *)contextBuffer {
    if (_contextBuffer == nil) {
        _contextBuffer = [[NSMutableString alloc] initWithString:@""];
    }
    
    return _contextBuffer;
}

- (void)setContextBuffer:(NSMutableString *)contextBuffer {
    _contextBuffer = [contextBuffer mutableCopy];
    if (_contextBuffer.length)
        [_contextBuffer replaceOccurrencesOfString:@"\0" withString:[NSString nullChar] options:0 range:NSMakeRange(0, 1)];
    [self.kme setContextBuffer:self.contextBuffer];
}

- (void)setUseNullChar:(BOOL)useNullChar {
    _useNullChar = YES;
    /*
    _useNullChar = useNullChar;
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setBool:_useNullChar forKey:@"KMUseNullCharKey"];
    [userData synchronize];
    */
}

- (BOOL)useNullChar {
    return YES;
    /*
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    _useNullChar = [userData boolForKey:@"KMUseNullCharKey"];
    return _useNullChar;
    */
}

- (void)awakeFromNib {
    [self setKeyboardsSubMenu];
    
    NSMenuItem *config = [self.menu itemWithTag:2];
    if (config)
        [config setAction:@selector(menuAction:)];
    
    NSMenuItem *osk = [self.menu itemWithTag:3];
    if (osk)
        [osk setAction:@selector(menuAction:)];
    
    NSMenuItem *about = [self.menu itemWithTag:4];
    if (about)
        [about setAction:@selector(menuAction:)];
}

- (void)setKeyboardsSubMenu {
    NSMenuItem *keyboards = [self.menu itemWithTag:1];
    if (keyboards) {
        KVKFile *kvk = nil;
        BOOL didSetKeyboard = NO;
        NSInteger itag = 1000;
        [keyboards.submenu removeAllItems];
        for (NSString *path in self.activeKeyboards) {
            NSDictionary *infoDict = [KMXFile infoDictionaryFromFilePath:path];
            if (!infoDict)
                continue;
            //NSString *str = [NSString stringWithFormat:@"%@ (%@)", [infoDict objectForKey:kKMKeyboardNameKey], [infoDict objectForKey:kKMKeyboardVersionKey]];
            NSString *str = [infoDict objectForKey:kKMKeyboardNameKey];
            NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:str action:@selector(menuAction:) keyEquivalent:@""];
            [item setTag:itag++];
            if ([path isEqualToString:self.selectedKeyboard]) {
                [item setState:NSOnState];
                KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
                [self setKmx:kmx];
                NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
                NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
                if (kvkFilename != nil) {
                    NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
                    if (kvkFilePath != nil)
                        kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
                }
                [self setKvk:kvk];
                [self setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
                [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];

                didSetKeyboard = YES;
            }
            else
                [item setState:NSOffState];

            [keyboards.submenu addItem:item];
        }
        
        if (keyboards.submenu.numberOfItems == 0) {
            NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:@"(None)" action:NULL keyEquivalent:@""];
            [keyboards.submenu addItem:item];
            [self setKmx:nil];
            [self setKvk:nil];
            [self setKeyboardName:nil];
            [self setKeyboardIcon:nil];
            [self setContextBuffer:nil];
            [self setSelectedKeyboard:nil];
        }
        else if (!didSetKeyboard) {
            [keyboards.submenu itemAtIndex:0].state = NSOnState;
            NSString *path = [self.activeKeyboards objectAtIndex:0];
            KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
            [self setKmx:kmx];
            NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
            NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
            if (kvkFilename != nil) {
                NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
                if (kvkFilePath != nil)
                    kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
            }
            [self setKvk:kvk];
            [self setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
            [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
            [self setContextBuffer:nil];
            [self setSelectedKeyboard:path];
        }
    }
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

- (NSString *)kvkFilePathFromFilename:(NSString *)kvkFilename {
    NSString *kvkFilePath = nil;
    NSArray *kvkFiles = [self KVKFiles];
    for (NSString *filePath in kvkFiles) {
        if ([[filePath lastPathComponent] isEqualToString:kvkFilename]) {
            kvkFilePath = filePath;
            break;
        }
    }
    
    return kvkFilePath;
}

- (NSWindowController *)oskWindow {
    if (!_oskWindow)
        _oskWindow = [[OSKWindowController alloc] initWithWindowNibName:@"OSKWindowController"];
    
    return _oskWindow;
}

- (void)showOSK {
    [[self.oskWindow window] makeKeyAndOrderFront:nil];
    [[self.oskWindow window] setLevel:NSStatusWindowLevel];
    [[self.oskWindow window] setTitle:self.oskWindowTitle];
}

- (void)showAboutWindow {
    [self.aboutWindow.window centerInParent];
    [self.aboutWindow.window makeKeyAndOrderFront:nil];
    [self.aboutWindow.window setLevel:NSFloatingWindowLevel];
}

- (NSWindowController *)aboutWindow_ {
    return _aboutWindow;
}

- (NSWindowController *)aboutWindow {
    if (_aboutWindow.window == nil) {
        _aboutWindow = [[KMAboutWindowController alloc] initWithWindowNibName:@"KMAboutWindowController"];
    }
    
    return _aboutWindow;
}

- (NSWindowController *)infoWindow_ {
    return _infoWindow;
}

- (NSWindowController *)infoWindow {
    if (_infoWindow.window == nil) {
        _infoWindow = [[KMInfoWindowController alloc] initWithWindowNibName:@"KMInfoWindowController"];
    }
    
    return _infoWindow;
}

- (NSWindowController *)kbHelpWindow_ {
    return _kbHelpWindow;
}

- (NSWindowController *)kbHelpWindow {
    if (_kbHelpWindow.window == nil) {
        _kbHelpWindow = [[KMKeyboardHelpWindowController alloc] initWithWindowNibName:@"KMKeyboardHelpWindowController"];
    }
    
    return _kbHelpWindow;
}

- (NSWindowController *)downloadKBWindow_ {
    return _downloadKBWindow;
}

- (NSWindowController *)downloadKBWindow {
    if (_downloadKBWindow.window == nil) {
        _downloadKBWindow = [[KMDownloadKBWindowController alloc] initWithWindowNibName:@"KMDownloadKBWindowController"];
    }
    
    return _downloadKBWindow;
}

- (void)alertDidEnd:(NSAlert *)alert returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    NSButton *button = (NSButton *)[alert.buttons objectAtIndex:0];
    if (button.tag == -1) {
        [_connection cancel];
    }
    else if (button.tag == 1) {
        [_downloadKBWindow close];
        if (self.configWindow.window != nil) {
            [self.configWindow.window makeKeyAndOrderFront:nil];
            if (![[self.configWindow.window childWindows] containsObject:self.infoWindow.window]) {
                [self.configWindow.window addChildWindow:self.infoWindow.window ordered:NSWindowAbove];
            }
            [self.infoWindow.window centerInParent];
            [self.infoWindow.window makeKeyAndOrderFront:nil];
        }
        else {
            [self.infoWindow.window centerInParent];
            [self.infoWindow.window makeKeyAndOrderFront:nil];
            [self.infoWindow.window setLevel:NSFloatingWindowLevel];
        }
        
        NSString *packagePath = [self.keyboardsPath stringByAppendingPathComponent:[self.downloadFilename stringByDeletingPathExtension]];
        [self.infoWindow setPackagePath:packagePath];
    }
    
    _downloadInfoView = nil;
    _connection = nil;
    _downloadFilename = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (NSAlert *)downloadInfoView {
    if (_downloadInfoView == nil) {
        _downloadInfoView = [[NSAlert alloc] init];
        [_downloadInfoView setMessageText:@"Downloading..."];
        [_downloadInfoView setInformativeText:@""];
        [_downloadInfoView addButtonWithTitle:@"Cancel"];
        [_downloadInfoView setAlertStyle:NSInformationalAlertStyle];
        [_downloadInfoView setAccessoryView:self.progressIndicator];
    }
    
    return _downloadInfoView;
}

- (NSProgressIndicator *)progressIndicator {
    if (_progressIndicator == nil) {
        _progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSMakeRect(0, 0, 300, 20)];
        [_progressIndicator setIndeterminate:NO];
        [_progressIndicator setMinValue:0];
        [_progressIndicator setMaxValue:100];
        [_progressIndicator setDoubleValue:0];
    }
    
    return _progressIndicator;
}

- (void)downloadKeyboardFromURL:(NSURL *)url {
    if (_connection == nil) {
        [_downloadInfoView setMessageText:@"Downloading..."];
        NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
        [button setTitle:@"Cancel"];
        [button setTag:-1];
        [self.progressIndicator setDoubleValue:0];
        NSURLRequest *request = [NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60];
        _receivedData = [[NSMutableData alloc] initWithLength:0];
        _connection = [[NSURLConnection alloc] initWithRequest:request delegate:self startImmediately:YES];
    }
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    [self.receivedData setLength:0];
    self.expectedBytes = [response expectedContentLength];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    [self.receivedData appendData:data];
    float progress = ((float)[self.receivedData length]/(float)self.expectedBytes)*100;
    [self.progressIndicator setDoubleValue:progress];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    _connection = nil;
    _downloadFilename = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection willCacheResponse:(NSCachedURLResponse *)cachedResponse {
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    NSString *filePath = [self.keyboardsPath stringByAppendingPathComponent:self.downloadFilename];
    [self.receivedData writeToFile:filePath atomically:YES];
    [self unzipFile:filePath];
    [[NSFileManager defaultManager] removeItemAtPath:filePath error:nil];
    [_downloadInfoView setMessageText:@"Download Complete"];
    NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
    [button setTitle:@"Done"];
    [button setTag:1];
    [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardDownloadCompletedNotification
                                                        object:self
                                                      userInfo:nil];
    _connection = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (void)handleKeyEvent:(NSEvent *)event {
    if (_oskWindow == nil)
        return;
    
    [_oskWindow.oskView handleKeyEvent:event];
}

- (BOOL)unzipFile:(NSString *)filePath {
    BOOL didUnzip = NO;
    NSString *fileName = filePath.lastPathComponent;
    NSString *folderName = [fileName stringByDeletingPathExtension];
    ZipArchive *za = [[ZipArchive alloc] init];
    if ([za UnzipOpenFile:filePath]) {
        didUnzip = [za UnzipFileTo:[self.keyboardsPath stringByAppendingPathComponent:folderName] overWrite:YES];
        [za UnzipCloseFile];
    }
    
    if (didUnzip) {
        [self installFontsAtPath:[self.keyboardsPath stringByAppendingPathComponent:folderName]];
    }
    
    return didUnzip;
}

- (NSString *)fontsPath {
    if (_fontsPath == nil) {
        BOOL isDir;
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES);
        
        if (paths.count == 1) {
            NSString *path = [[paths objectAtIndex:0] stringByAppendingPathComponent:@"Fonts"];
            if ([[NSFileManager defaultManager] fileExistsAtPath:path isDirectory:&isDir] && isDir)
                _fontsPath = [NSString stringWithString:path];
        }
    }
    
    return _fontsPath;
}

- (void)installFontsAtPath:(NSString *)path {
    NSString *fontsPath = self.fontsPath;
    if (fontsPath == nil)
        return;
    
    NSArray *fonts = [self FontFilesAtPath:path];
    for (NSString *srcPath in fonts) {
        NSString *destPath = [fontsPath stringByAppendingPathComponent:[srcPath lastPathComponent]];
        NSError *error;
        if ([[NSFileManager defaultManager] fileExistsAtPath:destPath])
            [[NSFileManager defaultManager] removeItemAtPath:destPath error:&error];
        
        if (error == nil)
            [[NSFileManager defaultManager] copyItemAtPath:srcPath toPath:destPath error:&error];
        
        if (error != nil)
            NSLog(@"Error = %@", error);
    }
}

- (NSArray *)FontFilesAtPath:(NSString *)path {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:path];
    NSMutableArray *fontFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"ttf"] || [extension isEqualToString:@"otf"])
            [fontFiles addObject:[path stringByAppendingPathComponent:filePath]];
    }
    
    return fontFiles;
}

@end
