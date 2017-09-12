//
//  KeyboardInfoViewController.h
//  KeymanEngine
//
//  Created by Serkan Kurt on 3/06/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KeyboardInfoViewController : UITableViewController <UIAlertViewDelegate>

@property (nonatomic, assign) NSUInteger keyboardCount;
@property (nonatomic, assign) NSInteger keyboardIndex;
@property (nonatomic, strong) NSString *keyboardID;
@property (nonatomic, strong) NSString *languageID;
@property (nonatomic, strong) NSString *keyboardVersion;
@property (nonatomic, strong) NSString *keyboardCopyright;
@property (assign) BOOL isCustomKeyboard;

@end
