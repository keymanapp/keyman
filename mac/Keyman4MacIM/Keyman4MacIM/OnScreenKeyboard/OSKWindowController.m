//
//  OSKWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 30/04/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "OSKWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMSettingsRepository.h"
#import "KMLogs.h"

@interface OSKWindowController ()
@property (nonatomic, strong) NSButton *helpButton;
@property (nonatomic, strong) NSTimer *modFlagsTimer;
@property (nonatomic, assign) BOOL xShiftState;
@property (nonatomic, assign) BOOL xAltState;
@property (nonatomic, assign) BOOL xCtrlState;
@end

@implementation OSKWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)dealloc {
  //[[NSNotificationCenter defaultCenter] removeObserver:self];
  [self stopTimer];
}

- (void)awakeFromNib {
  os_log_debug([KMLogs oskLog], "OSKWindowController awakeFromNib");
  // Keep the aspect ratio constant at its current value
  [self.window setAspectRatio:self.window.frame.size];
  NSSize size = self.window.frame.size;
  [self.window setMaxSize:NSMakeSize(size.width*1.6, size.height*1.6)];
  [self.window setMinSize:NSMakeSize(size.width*0.8, size.height*0.8)];
  [self.window setBackgroundColor:[NSColor colorWithSRGBRed:241.0/255.0 green:242.0/255.0 blue:242.0/255.0 alpha:1.0]];
  
  // # 1079: Versions of macOS prior to 10.10 do not support setting control size.
  // Safest to just not add the help button at all on the OSK.
  NSButton *helpBtn = [[NSButton alloc] initWithFrame:NSMakeRect(0, 0, 17, 17)];
  if (helpBtn && [helpBtn respondsToSelector:@selector(setControlSize:)]) {
    _helpButton = helpBtn;
    [_helpButton setTitle:@""];
    [_helpButton setBezelStyle:NSHelpButtonBezelStyle];
    [_helpButton setControlSize:NSControlSizeMini];
    [_helpButton setAction:@selector(helpAction:)];
    [_helpButton setEnabled:[self hasHelpDocumentation]];
    [self.window addViewToTitleBar:_helpButton positionX:NSWidth(self.window.frame) - NSWidth(_helpButton.frame) -10];
  }
}

- (void)windowDidLoad {
  os_log_debug([KMLogs oskLog], "OSKWindowController windowDidLoad");
  [super windowDidLoad];
  //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidResize:) name:NSWindowDidResizeNotification object:self.window];
  [self.oskView setKvk:[self.AppDelegate kvk]];
  [self startTimerWithTimeInterval:0.1];
  // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
}

- (void)windowDidResize:(NSNotification *)notification {
  os_log_debug([KMLogs oskLog], "OSKWindowController windowDidResize");
  [self.oskView resizeOSKLayout];
}

- (void)windowWillClose:(NSNotification *)notification {
  [KMSettingsRepository.shared writeShowOskOnActivate:NO];
  os_log_debug([KMLogs oskLog], "OSKWindowController windowWillClose, updating settings writeShowOsk to NO");
}

- (void)helpAction:(id)sender {
  NSString *kvkPath = [self AppDelegate].kvk.filePath;
  if (!kvkPath)
    return;
  
  NSString *packagePath = [kvkPath stringByDeletingLastPathComponent];
  if (packagePath != nil) {
    if (self.AppDelegate.kbHelpWindow_.window != nil)
      [self.AppDelegate.kbHelpWindow_ close];
    
    [self.AppDelegate.kbHelpWindow.window centerInParent];
    [self.AppDelegate.kbHelpWindow.window makeKeyAndOrderFront:nil];
    [self.AppDelegate.kbHelpWindow.window setLevel:NSFloatingWindowLevel];
    [self.AppDelegate.kbHelpWindow setPackagePath:packagePath];
  }
}

- (void)resetOSK {
  os_log_debug([KMLogs oskLog], "OSKWindowController windowDidLoad");
  [self.oskView setKvk:[self.AppDelegate kvk]];
  [self.oskView resetOSK];
  if (_helpButton) {
    [_helpButton setEnabled:[self hasHelpDocumentation]];
  }
}

- (void)startTimerWithTimeInterval:(NSTimeInterval)interval {
  if (_modFlagsTimer == nil) {
    TimerTarget *timerTarget = [[TimerTarget alloc] init];
    timerTarget.target = self;
    _modFlagsTimer = [NSTimer scheduledTimerWithTimeInterval:interval
                                                      target:timerTarget
                                                    selector:@selector(timerAction:)
                                                    userInfo:nil
                                                     repeats:YES];
  }
}

- (void)stopTimer {
  if (_modFlagsTimer != nil) {
    [_modFlagsTimer invalidate];
    _modFlagsTimer = nil;
  }
}

- (void)timerAction:(NSTimer *)timer {
  UInt32 modifiers = GetCurrentKeyModifiers();
  if ((modifiers & shiftKey) == shiftKey) {
    [self.oskView setShiftState:YES];
    self.xShiftState = YES;
  }
  else {
    [self.oskView setShiftState:NO];
    if (self.xShiftState)
      [self.oskView setOskShiftState:NO];
    self.xShiftState = NO;
  }
  
  if ((modifiers & optionKey) == optionKey) {
    [self.oskView setAltState:YES];
    self.xAltState = YES;
  }
  else {
    [self.oskView setAltState:NO];
    if (self.xAltState)
      [self.oskView setOskAltState:NO];
    self.xAltState = NO;
  }
  
  if ((modifiers & controlKey) == controlKey) {
    [self.oskView setCtrlState:YES];
    self.xCtrlState = YES;
  }
  else {
    [self.oskView setCtrlState:NO];
    if (self.xCtrlState)
      [self.oskView setOskCtrlState:NO];
    self.xCtrlState = NO;
  }
}

- (BOOL)hasHelpDocumentation {
  NSString *kvkPath = [self AppDelegate].kvk.filePath;
  if (!kvkPath)
    return NO;
  
  NSString *packagePath = [kvkPath stringByDeletingLastPathComponent];
  if (packagePath != nil) {
    NSString *welcomeFile = [packagePath stringByAppendingPathComponent:@"welcome.htm"];
    if ([[NSFileManager defaultManager] fileExistsAtPath:welcomeFile])
      return YES;
  }
  
  return NO;
}

@end
