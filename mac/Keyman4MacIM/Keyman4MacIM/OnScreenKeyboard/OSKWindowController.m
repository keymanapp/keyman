//
//  OSKWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 30/04/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "OSKWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMInputMethodLifecycle.h"
#import "KMSettingsRepository.h"
#import "KMLogs.h"

@interface OSKWindowController ()
@property (nonatomic, strong) NSButton *helpButton;
@property (nonatomic, strong) NSTimer *modFlagsTimer;
@property (nonatomic, assign) BOOL previousShiftState;
@property (nonatomic, assign) BOOL previousOptionState;
@property (nonatomic, assign) BOOL previousControlState;
@end

@implementation OSKWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)dealloc {
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
  [self.oskView setKvk:[self.AppDelegate kvk]];
  [self startTimerWithTimeInterval:0.1];
  //  [self startTimerToTrackPhysicalModifierState:0.1];
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

/**
 * Called repeatedly by timer to track the state of the physical modifier keys.
 * Changes in state are reported to the OSKView, so it can make corresponding updates to the OSK's appearance
 * and apply the state as necessary when keys on the OSK are clicked.
 */
- (void)timerAction:(NSTimer *)timer {
  UInt32 modifiers = GetCurrentKeyModifiers();
  if ((modifiers & shiftKey) == shiftKey) {
    [self.oskView setPhysicalShiftState:YES];
    self.previousShiftState = YES;
  }
  else {
    [self.oskView setPhysicalShiftState:NO];
    if (self.previousShiftState)
      [self.oskView setOskShiftState:NO];
    self.previousShiftState = NO;
  }
  if ((modifiers & optionKey) == optionKey) {
    [self.oskView setPhysicalOptionState:YES];
    self.previousOptionState = YES;
  }
  else {
    [self.oskView setPhysicalOptionState:NO];
    if (self.previousOptionState)
      [self.oskView setOskOptionState:NO];
    self.previousOptionState = NO;
  }
  
  if ((modifiers & controlKey) == controlKey) {
    [self.oskView setPhysicalControlState:YES];
    self.previousControlState = YES;
  }
  else {
    [self.oskView setPhysicalControlState:NO];
    if (self.previousControlState)
      [self.oskView setOskControlState:NO];
    self.previousControlState = NO;
  }
}

/*
- (void)trackPhysicalModifierKeyState:(NSTimer *)timer {
  UInt32 modifiers = GetCurrentKeyModifiers();
  
  [self trackPhysicalShiftKeyState: modifiers];
  [self trackPhysicalOptionKeyState: modifiers];
  [self trackPhysicalControlKeyState: modifiers];
}

- (void)trackPhysicalShiftKeyState:(UInt32) modifiers {
  if ((modifiers & shiftKey) == shiftKey) {
    [self.oskView setPhysicalShiftState:YES];
    self.previousShiftState = YES;
  }
  else {
    [self.oskView setPhysicalShiftState:NO];
    if (self.previousShiftState) {
      [self.oskView setOskShiftState:NO];
    }
    self.previousShiftState = NO;
  }
}

- (void)trackPhysicalOptionKeyState:(UInt32) modifiers {
  if ((modifiers & optionKey) == optionKey) {
    [self.oskView setPhysicalOptionState:YES];
    self.previousOptionState = YES;
  }
  else {
    [self.oskView setPhysicalOptionState:NO];
    if (self.previousOptionState) {
      [self.oskView setOskOptionState:NO];
    }
    self.previousOptionState = NO;
  }
}

- (void)trackPhysicalControlKeyState:(UInt32) modifiers {
  if ((modifiers & controlKey) == controlKey) {
    [self.oskView setPhysicalControlState:YES];
    self.previousControlState = YES;
  }
  else {
    [self.oskView setPhysicalControlState:NO];
    if (self.previousControlState) {
      [self.oskView setOskControlState:NO];
    }
    self.previousControlState = NO;
  }
}
*/

- (BOOL)hasHelpDocumentation {
  NSString *kvkPath = [self AppDelegate].kvk.filePath;
  if (!kvkPath) {
    return NO;
  }
  
  NSString *packagePath = [kvkPath stringByDeletingLastPathComponent];
  if (packagePath != nil) {
    NSString *welcomeFile = [packagePath stringByAppendingPathComponent:@"welcome.htm"];
    if ([[NSFileManager defaultManager] fileExistsAtPath:welcomeFile]) {
      return YES;
    }
  }
  
  return NO;
}

@end
