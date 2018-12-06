//
//  KMConfigurationWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 24/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMConfigurationWindowController.h"
#import "KMDownloadKBWindowController.h"

@interface KMConfigurationWindowController ()
@property (nonatomic, weak) IBOutlet NSTableView *tableView;
@property (nonatomic, weak) IBOutlet WebView *webView;
@property (nonatomic, weak) IBOutlet NSButton *alwaysShowOSKCheckBox;
@property (nonatomic, weak) IBOutlet NSButton *useVerboseLoggingCheckBox;
@property (nonatomic, weak) IBOutlet NSTextField *verboseLoggingInfo;
@property (nonatomic, strong) NSMutableArray *tableContents;
@property (nonatomic, strong) NSTimer *reloadTimer;
@property (nonatomic, strong) NSDate *lastReloadDate;
@property (nonatomic, strong) NSAlert *deleteAlertView;
@property (nonatomic, strong) NSAlert *confirmKmpInstallAlertView;
@end

@implementation KMConfigurationWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithWindowNibName:(NSString *)windowNibName {
    self = [super initWithWindowNibName:windowNibName];
    if (self) {
        if (self.AppDelegate.infoWindow_.window != nil) {
            [self.window addChildWindow:self.AppDelegate.infoWindow_.window ordered:NSWindowAbove];
            [self.AppDelegate.infoWindow_.window centerInParent];
        }
        else if (self.AppDelegate.downloadKBWindow_.window != nil) {
            [self.window addChildWindow:self.AppDelegate.downloadKBWindow_.window ordered:NSWindowAbove];
            [self.AppDelegate.downloadKBWindow_.window centerInParent];
        }
        
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(timerAction:) name:kKeymanKeyboardDownloadCompletedNotification object:nil];
    }
    
    return self;
}

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [self stopTimer];
}

- (void)windowDidLoad {
    [super windowDidLoad];
    [self.window center];
    [_tableView registerForDraggedTypes:@[NSFilenamesPboardType]];
    _lastReloadDate = [NSDate date];
    [self startTimer];
    
    [self.webView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
    [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"https://help.keyman.com/products/macosx/"]]];

    [self.alwaysShowOSKCheckBox setState:(self.AppDelegate.alwaysShowOSK ? NSOnState : NSOffState)];
    [self.useVerboseLoggingCheckBox setState:(self.AppDelegate.useVerboseLogging ? NSOnState : NSOffState)];
}

- (void)setTableView:(NSTableView *)tableView {
    _tableView = tableView;
    NSMenu *menu = [[NSMenu alloc] init];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:@"Refresh" action:@selector(refreshAction:) keyEquivalent:@""];
    [menu addItem:item];
    [_tableView setMenu:menu];
}

- (void)refreshAction:(id)sender {
    [self.AppDelegate setKmxFileList:nil];
    [self setTableContents:nil];
    [self saveActiveKeyboards];
    [self.tableView reloadData];
    _lastReloadDate = [NSDate date];
}

- (NSArray *)tableContents {
    if (_tableContents == nil) {
        _tableContents = [[NSMutableArray alloc] initWithCapacity:0];
        for (int i = 0; i < self.kmxFileList.count; i++) {
            id obj = [self.kmxFileList objectAtIndex:i];
            if ([obj isKindOfClass:[NSArray class]]) {
                NSArray *pArray = (NSArray *)obj;
                NSString *packageFolder = [self packageFolderFromPath:[pArray objectAtIndex:0]];
                NSString *packageName = [self packageNameFromFolder:packageFolder];
                [_tableContents addObject:[NSDictionary dictionaryWithObjectsAndKeys:packageName, @"HeaderTitle", nil]];
                for (NSString *path in pArray) {
                    NSDictionary *info = [KMXFile infoDictionaryFromFilePath:path];
                    if (info)
                        [_tableContents addObject:info];
                }
            }
            else {
                NSString *path = (NSString *)obj;
                NSDictionary *info = [KMXFile infoDictionaryFromFilePath:path];
                if (info)
                    [_tableContents addObject:info];
            }
        }
    }
    
    return _tableContents;
}

- (NSString *)kmxFilePathAtIndex:(NSUInteger)index {
    return [self.AppDelegate kmxFilePathAtIndex:index];
}

- (NSString *)packagePathAtIndex:(NSUInteger)index {
    return [self.AppDelegate packagePathAtIndex:index];
}

- (NSInteger)indexForPackageFolder:(NSString *)packageFolder {
    return [self.AppDelegate indexForPackageFolder:packageFolder];
}

- (NSString *)packageFolderFromPath:(NSString *)path {
    return [self.AppDelegate packageFolderFromPath:path];
}

- (NSString *)packageNameFromFolder:(NSString *)packageFolder {
    return [self.AppDelegate packageNameFromFolder:packageFolder];
}

- (NSString *)keyboardsPath {
    return self.AppDelegate.keyboardsPath;
}

- (NSArray *)kmxFileList {
    return self.AppDelegate.kmxFileList;
}

- (NSMutableArray *)activeKeyboards {
    return self.AppDelegate.activeKeyboards;
}

- (void)saveActiveKeyboards {
    [self.AppDelegate saveActiveKeyboards];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView {
    return self.tableContents.count;
}

- (NSIndexSet *)tableView:(NSTableView *)tableView selectionIndexesForProposedSelection:(NSIndexSet *)proposedSelectionIndexes {
    return 0;
}

- (NSTableRowView *)tableView:(NSTableView *)tableView rowViewForRow:(NSInteger)row {
    NSDictionary *info = [self.tableContents objectAtIndex:row];
    BOOL isHeader = ([info objectForKey:@"HeaderTitle"] != nil);
    if (isHeader) {
        NSTableRowView *rowView = [[NSTableRowView alloc] init];
        NSTextField *textField = [[NSTextField alloc] initWithFrame:NSMakeRect(4, 4, tableView.frame.size.width-86, tableView.rowHeight-8)];
        [textField setEditable:NO];
        [textField setBordered:NO];
        [textField setBackgroundColor:[NSColor clearColor]];
        [textField setAlignment:NSLeftTextAlignment];
        [textField setFont:[NSFont systemFontOfSize:tableView.rowHeight*0.5]];
        [textField setTextColor:[NSColor colorWithSRGBRed:0.0 green:0.0 blue:0.1 alpha:1.0]];
        [textField setStringValue:[info objectForKey:@"HeaderTitle"]];
        [rowView addSubview:textField];
        return rowView;
    }
    
    return nil;
}

- (void)tableView:(NSTableView *)tableView didAddRowView:(NSTableRowView *)rowView forRow:(NSInteger)row {
    NSDictionary *info = [self.tableContents objectAtIndex:row];
    BOOL isHeader = ([info objectForKey:@"HeaderTitle"] != nil);
    if (isHeader)
        [rowView setBackgroundColor:[NSColor colorWithSRGBRed:186.0/255.0 green:211.0/255.0 blue:1.0 alpha:1.0]];
}

- (NSView *)tableView:(NSTableView *)tableView viewForTableColumn:(NSTableColumn *)tableColumn row:(NSInteger)row {
    NSDictionary *info = [self.tableContents objectAtIndex:row];
    NSString *identifier = [tableColumn identifier];
    NSString *headerTitle = [info objectForKey:@"HeaderTitle"];
    BOOL isHeader = (headerTitle != nil);
    BOOL isOthers = NO;
    NSString *kmxFilePath = [self kmxFilePathAtIndex:row];
    if (kmxFilePath != nil)
        isOthers = [[self packageFolderFromPath:kmxFilePath] isEqualToString:@"Others"];
    else if (isHeader && [headerTitle isEqualToString:@"Others"])
        isOthers = YES;

    if ([identifier isEqualToString:@"Column1"]) {
        KMConfigColumn1CellView *cellView = [tableView makeViewWithIdentifier:identifier owner:self];
        
        if (isHeader)
            [cellView setHidden:YES];
        else {
            [cellView setHidden:NO];
            cellView.imageView.objectValue = [info objectForKey:kKMKeyboardIconKey];
            [cellView.checkBox setTag:row];
            [cellView.checkBox setAction:@selector(checkBoxAction:)];
            [cellView.checkBox setState:([self.activeKeyboards containsObject:[self kmxFilePathAtIndex:row]])?NSOnState:NSOffState];
        }
        
        return cellView;
    }
    else if ([identifier isEqualToString:@"Column2"]) {
        NSTableCellView *cellView = [tableView makeViewWithIdentifier:identifier owner:self];
        if (!isHeader) {
            [cellView setHidden:NO];
            //cellView.textField.stringValue = [NSString stringWithFormat:@"%@ (%@)", [info objectForKey:kKMKeyboardNameKey], [info objectForKey:kKMKeyboardVersionKey]];
            cellView.textField.stringValue = [info objectForKey:kKMKeyboardNameKey];
        }
        else {
            cellView.textField.stringValue = @"";
            [cellView setHidden:YES];
        }
        
        return cellView;
    }
    else if ([identifier isEqualToString:@"Column3"]) {
        KMConfigColumn3CellView *cellView = [tableView makeViewWithIdentifier:identifier owner:self];
        
        if ((!isHeader && !isOthers) || (isHeader && isOthers)) {
            [cellView.removeButton setHidden:YES];
            [cellView.infoButton setHidden:YES];
            [cellView.helpButton setHidden:YES];
        }
        else if (isOthers) {
            [cellView.removeButton setHidden:NO];
            [cellView.infoButton setHidden:YES];
            [cellView.helpButton setHidden:YES];
        }
        else {
            [cellView.removeButton setHidden:NO];
            
            [cellView.infoButton setHidden:NO];
            [cellView.infoButton setTag:row];
            [cellView.infoButton setAction:@selector(infoAction:)];
            [cellView.infoButton setBordered:NO];
            [cellView.infoButton setWantsLayer:YES];
            CALayer *btnLayer = [cellView.infoButton layer];
            btnLayer.backgroundColor = [NSColor clearColor].CGColor;
            btnLayer.cornerRadius = 9.5;
            
            [cellView.helpButton setHidden:NO];
            [cellView.helpButton setTag:row];
            [cellView.helpButton setAction:@selector(helpAction:)];
            [cellView.helpButton setEnabled:[self hasHelpDocumentation:row]];
        }
        
        if (![cellView.removeButton isHidden]) {
            [cellView.removeButton setTag:row];
            [cellView.removeButton setAction:@selector(removeAction:)];
            [cellView.removeButton setBordered:NO];
            [cellView.removeButton setWantsLayer:YES];
            CALayer *btnLayer = [cellView.removeButton layer];
            btnLayer.backgroundColor = [NSColor whiteColor].CGColor;
            btnLayer.cornerRadius = 9.5;
            btnLayer.borderWidth = 1.0;
            btnLayer.borderColor = [NSColor grayColor].CGColor;
        }
        
        return cellView;
    }
    
    return nil;
}

- (NSDragOperation)tableView:(NSTableView*)tv validateDrop:(id <NSDraggingInfo>)info proposedRow:(NSInteger)row proposedDropOperation:(NSTableViewDropOperation)op {
    return NSDragOperationCopy;
}

- (BOOL)tableView:(NSTableView *)tableView acceptDrop:(id<NSDraggingInfo>)info row:(NSInteger)row dropOperation:(NSTableViewDropOperation)dropOperation {
    NSPasteboard *pboard = [info draggingPasteboard];
    NSArray *filenames = [pboard propertyListForType:NSFilenamesPboardType];
    NSMutableArray *kmpFiles = nil;
    for (NSString *filename in filenames) {
        if ([[filename lastNChars:4] isEqualTo:@".kmp"]) {
            if (kmpFiles == nil)
                kmpFiles = [NSMutableArray arrayWithObject:filename];
            else
                [kmpFiles addObject:filename];
        }
    }
    
    for (NSString *kmpFile in kmpFiles) {
        if ([self.AppDelegate unzipFile:kmpFile])
            [self refreshAction:nil];
        else
            [kmpFiles removeObject:kmpFile];
    }
    
    if (kmpFiles.count < 1)
        kmpFiles = nil;
    
    return (kmpFiles==nil?NO:YES);
}

- (void)checkBoxAction:(id)sender {
    NSButton *checkBox = (NSButton *)sender;
    NSString *kmxFilePath = [self kmxFilePathAtIndex:checkBox.tag];
    if (checkBox.state == NSOnState) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Adding active keyboard: %@", kmxFilePath);
        [self.activeKeyboards addObject:kmxFilePath];
        [self saveActiveKeyboards];
    }
    else if (checkBox.state == NSOffState) {
        [self.activeKeyboards removeObject:kmxFilePath];
        [self saveActiveKeyboards];
    }
}

- (void)infoAction:(id)sender {
    NSButton *infoButton = (NSButton *)sender;
    NSString *packagePath = [self packagePathAtIndex:infoButton.tag];
    if (packagePath != nil) {
        if (self.AppDelegate.infoWindow_.window != nil)
            [self.AppDelegate.infoWindow_ close];
        
        [self.window addChildWindow:self.AppDelegate.infoWindow.window ordered:NSWindowAbove];
        [self.AppDelegate.infoWindow.window centerInParent];
        [self.AppDelegate.infoWindow.window makeKeyAndOrderFront:nil];
        [self.AppDelegate.infoWindow setPackagePath:packagePath];
    }
}

- (void)helpAction:(id)sender {
    NSButton *helpButton = (NSButton *)sender;
    NSString *packagePath = [self packagePathAtIndex:helpButton.tag];
    if (packagePath != nil) {
        if (self.AppDelegate.kbHelpWindow_.window != nil)
            [self.AppDelegate.kbHelpWindow_ close];
        
        [self.window addChildWindow:self.AppDelegate.kbHelpWindow.window ordered:NSWindowAbove];
        [self.AppDelegate.kbHelpWindow.window centerInParent];
        [self.AppDelegate.kbHelpWindow.window makeKeyAndOrderFront:nil];
        [self.AppDelegate.kbHelpWindow setPackagePath:packagePath];
    }
}

- (void)removeAction:(id)sender {
    NSButton *removeButton = (NSButton *)sender;
    NSDictionary *info = [self.tableContents objectAtIndex:removeButton.tag];
    NSString *msg;
    if ([info objectForKey:@"HeaderTitle"] != nil)
        msg = [NSString stringWithFormat:@"This action will permanently delete '%@'.", [info objectForKey:@"HeaderTitle"]];
    else
        msg = [NSString stringWithFormat:@"This action will permanently delete '%@'.", [info objectForKey:kKMKeyboardNameKey]];
    
    [self.deleteAlertView setMessageText:msg];
    [self.deleteAlertView beginSheetModalForWindow:self.window
                                      modalDelegate:self
                                     didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                        contextInfo:(__bridge void *)([NSNumber numberWithInteger:removeButton.tag])];
}

- (IBAction)downloadAction:(id)sender {
    if (self.AppDelegate.infoWindow_.window != nil)
        [self.AppDelegate.infoWindow_ close];
    
    if (self.AppDelegate.kbHelpWindow_.window != nil)
        [self.AppDelegate.kbHelpWindow_ close];
    
    [self.window addChildWindow:self.AppDelegate.downloadKBWindow.window ordered:NSWindowAbove];
    [self.AppDelegate.downloadKBWindow.window centerInParent];
    [self.AppDelegate.downloadKBWindow.window makeKeyAndOrderFront:nil];
}

- (IBAction)alwaysShowOSKCheckBoxAction:(id)sender {
    NSButton *checkBox = (NSButton *)sender;
    [self.AppDelegate setAlwaysShowOSK:(checkBox.state == NSOnState)];
}

- (IBAction)useVerboseLoggingCheckBoxAction:(id)sender {
    NSButton *checkBox = (NSButton *)sender;
    BOOL verboseLoggingOn = checkBox.state == NSOnState;
    [self.AppDelegate setUseVerboseLogging:verboseLoggingOn];
    [self.verboseLoggingInfo setHidden:!verboseLoggingOn];
}

- (void)handleRequestToInstallPackage:(KMPackage *) package {
    NSString *infoFmt = NSLocalizedString(@"Do you want the Keyman Input Method to install this Package?\rFile: %@", @"Alert informative text when user double-clicks a KMP file. Parameter is the name of the KMP file.");
    [self.confirmKmpInstallAlertView setInformativeText:[NSString localizedStringWithFormat:infoFmt, package.getOrigKmpFilename]];
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Asking user to confirm installation of %@...", package.getOrigKmpFilename);
        NSLog(@"KMP - temp file name: %@", package.getTempKmpFilename);
    }
    
    [self.confirmKmpInstallAlertView beginSheetModalForWindow:self.window
                                                modalDelegate:self
                                               didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                                  contextInfo:(__bridge void *)(package)];
}

- (void)installPackageFile:(NSString *)kmpFile {
    // kmpFile could be a temp file (in fact, it always is!), so don't display the name.
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"KMP - Ready to unzip/install Package File: %@", kmpFile);
    }
    
    BOOL didUnzip = [self.AppDelegate unzipFile:kmpFile];
    
    if (!didUnzip) {
        NSAlert *failure = [[NSAlert alloc] init];
        [failure addButtonWithTitle:NSLocalizedString(@"OK", @"Alert button")];
        [failure setMessageText:NSLocalizedString(@"Failed to unzip Keyman Package!", @"Alert message when user double-clicks a KMP file that cannot be unzipped.")];
        [failure setIcon:[[NSBundle mainBundle] imageForResource:@"logo.png"]];
        [failure setAlertStyle:NSWarningAlertStyle];
        [failure beginSheetModalForWindow:self.window
                            modalDelegate:self
                           didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                              contextInfo:nil];
    }
    else if ([self.AppDelegate debugMode]) {
        NSLog(@"Completed installation of KMP file.");
    }
}

- (void)startTimer {
    if (_reloadTimer == nil) {
        TimerTarget *timerTarget = [[TimerTarget alloc] init];
        timerTarget.target = self;
        _reloadTimer = [NSTimer scheduledTimerWithTimeInterval:5.0f
                                                     target:timerTarget
                                                      selector:@selector(timerAction:)
                                                   userInfo:nil
                                                    repeats:YES];
    }
}

- (void)stopTimer {
    if (_reloadTimer != nil) {
        [_reloadTimer invalidate];
        _reloadTimer = nil;
    }
}

- (void)timerAction:(NSTimer *)timer {
    if ([self shouldReloadData]) {
        [self.AppDelegate setKmxFileList:nil];
        [self setTableContents:nil];
        [self saveActiveKeyboards];
        [self.tableView reloadData];
        _lastReloadDate = [NSDate date];
    }
}

- (BOOL)shouldReloadData {
    NSDictionary *attrs = [[NSFileManager defaultManager] attributesOfItemAtPath:self.keyboardsPath error:nil];
    NSDate *lastModDate = [attrs fileModificationDate];
    return ([lastModDate compare:_lastReloadDate] == NSOrderedDescending);
}

- (BOOL)hasHelpDocumentation:(NSUInteger)row {
    NSString *packagePath = [self packagePathAtIndex:row];
    if (packagePath != nil) {
        NSString *welcomeFile = [packagePath stringByAppendingPathComponent:@"welcome.htm"];
        if ([[NSFileManager defaultManager] fileExistsAtPath:welcomeFile])
            return YES;
    }
    
    return NO;
}

- (NSAlert *)deleteAlertView {
    if (_deleteAlertView == nil) {
        _deleteAlertView = [[NSAlert alloc] init];
        [_deleteAlertView setMessageText:@"This action will permanently delete this keyboard."];
        [_deleteAlertView setInformativeText:@"Press OK to continue."];
        [_deleteAlertView addButtonWithTitle:@"OK"];
        [_deleteAlertView addButtonWithTitle:@"Cancel"];
        [_deleteAlertView setAlertStyle:NSWarningAlertStyle];
        [_deleteAlertView setIcon:[[NSBundle mainBundle] imageForResource:@"logo.png"]];
    }
    
    return _deleteAlertView;
}

- (NSAlert *)confirmKmpInstallAlertView {
    if (_confirmKmpInstallAlertView == nil) {
        _confirmKmpInstallAlertView = [[NSAlert alloc] init];
        [_confirmKmpInstallAlertView addButtonWithTitle:NSLocalizedString(@"Yes", @"Alert button")];
        [_confirmKmpInstallAlertView addButtonWithTitle:NSLocalizedString(@"No", @"Alert button")];
        [_confirmKmpInstallAlertView setMessageText:NSLocalizedString(@"Install Keyman Package?", @"Alert message text when user double-clicks a KMP file.")];
        [_confirmKmpInstallAlertView setAlertStyle:NSInformationalAlertStyle];
        [_confirmKmpInstallAlertView setIcon:[[NSBundle mainBundle] imageForResource:@"logo.png"]];
    }
    
    return _confirmKmpInstallAlertView;
}

- (void)alertDidEnd:(NSAlert *)alert returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"User responded to NSAlert");
    }
    if (alert == _deleteAlertView) {
        if (returnCode == NSAlertFirstButtonReturn) { // OK
            [self deleteFileAtIndex:(__bridge NSNumber *)contextInfo];
        }
        
        _deleteAlertView = nil;
    }
    else if (alert == _confirmKmpInstallAlertView) {
        KMPackage *package = (__bridge KMPackage *)contextInfo;
        if ([self.AppDelegate debugMode]) {
            NSLog(@"KMP - Temp file: %@", package.getTempKmpFilename);
        }
        if (returnCode == NSAlertFirstButtonReturn) { // Yes
            [self installPackageFile: package.getTempKmpFilename];
        }
        
        [package releaseTempKMPFile];
        _confirmKmpInstallAlertView = nil;
    }
    // else, just a message - nothing to do.
}

- (void)deleteFileAtIndex:(NSNumber *) n {
    NSInteger index = [n integerValue];
    NSString *path2Remove = nil;
    NSString *kmxFilePath = [self kmxFilePathAtIndex:index];
    if (kmxFilePath == nil) {
        kmxFilePath = [self kmxFilePathAtIndex:index+1];
        path2Remove = [[self keyboardsPath] stringByAppendingPathComponent:[self packageFolderFromPath:kmxFilePath]];
    }
    else {
        path2Remove = kmxFilePath;
    }
    
    NSError *error;
    [[NSFileManager defaultManager] removeItemAtPath:path2Remove error:&error];
    if (error == nil) {
        [self performSelector:@selector(timerAction:) withObject:nil afterDelay:1.0];
    }
}
@end
