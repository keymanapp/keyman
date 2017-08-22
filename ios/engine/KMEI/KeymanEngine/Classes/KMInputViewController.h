//
//  KMInputViewController.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

typedef enum {
    GKTapSwitchToNextKeyboard = 0,
    GKTapSwitchToNextInputMethod,
    GKTapDoNothing
} GlobeKeyTapBehaviour;

typedef enum {
    MenuShowAlways = 0,
    MenuShowIfMultipleKeyboards,
    MenuShowNever
} MenuBehaviour;

@interface KMInputViewController : UIInputViewController

@property (strong, nonatomic) UIImageView *topBarImageView;
@property (strong, nonatomic) NSString *menuCloseButtonTitle;
@property (assign) BOOL isInputClickSoundEnabled;
@property (assign) GlobeKeyTapBehaviour globeKeyTapBehaviour;
@property (assign) MenuBehaviour menuBehaviour;

- (BOOL)hasFullAccess;
+ (BOOL)isPortrait;
+ (NSUInteger)topBarHeight;

@end
