//
//  LanguageDetailViewController.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface LanguageDetailViewController : UITableViewController <UIAlertViewDelegate> {
    NSInteger languageIndex;
    NSString *languageName;
    NSString *languageID;
    NSArray *keyboards;
}

@property (nonatomic, assign) NSInteger languageIndex;
@property (nonatomic, strong) NSString *languageName;
@property (nonatomic, strong) NSString *languageID;
@property (nonatomic, strong) NSArray *keyboards;

@end
