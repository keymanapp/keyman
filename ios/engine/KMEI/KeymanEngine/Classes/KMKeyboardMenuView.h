//
//  KMKeyboardMenuView.h
//  KeymanEngine
//
//  Created by Serkan Kurt on 18/03/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "KMInputViewController.h"

@interface KMKeyboardMenuView : UIView <UITableViewDelegate, UITableViewDataSource, UIGestureRecognizerDelegate>

- (id)initWithKeyFrame:(CGRect)keyFrame inputViewController:(KMInputViewController *)inputViewController closeButtonTitle:(NSString *)closeButtonTitle;

@end
