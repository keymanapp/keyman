//
//  GetStartedViewController.m
//  Keyman
//
//  Created by Serkan Kurt on 22/08/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "GetStartedViewController.h"
#import "AppDelegate.h"
#import "Keyman.h"
#import "MainViewController.h"
#import "SetUpViewController.h"

@interface GetStartedViewController ()
@property (strong, nonatomic) IBOutlet UINavigationItem *navItem;
@property (strong, nonatomic) IBOutlet UITableView *tableView;
@end

@implementation GetStartedViewController
@synthesize mainViewController;

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)viewDidLoad {
    [super viewDidLoad];
    
    [[NSNotificationCenter defaultCenter]addObserver:self
                                            selector:@selector(refreshTable)
                                                name:UIApplicationDidBecomeActiveNotification
                                              object:nil];
    
    [self.tableView setScrollEnabled:NO];
    
    UIImageView *icon = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"887-notepad.png"]];
    self.navItem.leftBarButtonItem = [[UIBarButtonItem alloc] initWithCustomView:icon];
    
    if ([self.tableView respondsToSelector:@selector(setSeparatorInset:)])
        [self.tableView setSeparatorInset:UIEdgeInsetsMake(0, 0, 0, 15)];
    CGRect frame = CGRectMake(0, 0, self.tableView.frame.size.width, 1);
    UIView *header = [[UIView alloc] initWithFrame:frame];
    [header setBackgroundColor:[UIColor colorWithRed:0.0 green:0.5 blue:1.0 alpha:1.0]];
    
    UIView *line = [[UIView alloc] initWithFrame:frame];
    [line setBackgroundColor:[UIColor colorWithRed:0.0 green:0.5 blue:1.0 alpha:1.0]];
    
    frame.size.height = 63;
    
    UISwitch *dontShowAgainSwitch = [[UISwitch alloc] init];
    CGRect switchFrame = dontShowAgainSwitch.frame;
    
    CGFloat width = 120;
    CGFloat x = (frame.size.width - width - switchFrame.size.width)/2.0;
    CGRect labelFrame = CGRectMake(x, 0, width, frame.size.height);
    UILabel *label = [[UILabel alloc] initWithFrame:labelFrame];
    [label setText:@"Don't show again"];
    [label setFont:[label.font fontWithSize:12.0]];
    [label setTextAlignment:NSTextAlignmentCenter];
    [label setBackgroundColor:[UIColor clearColor]];
    
    switchFrame.origin.x = x + width;
    switchFrame.origin.y = (frame.size.height - dontShowAgainSwitch.frame.size.height)/2.0;
    [dontShowAgainSwitch setFrame:switchFrame];
    [dontShowAgainSwitch setOn:[self dontShowGetStarted]];
    [dontShowAgainSwitch addTarget:self action:@selector(switchValueChanged:) forControlEvents:UIControlEventValueChanged];
    
    UIView *footer = [[UIView alloc] initWithFrame:frame];
    [footer addSubview:label];
    [footer addSubview:dontShowAgainSwitch];
    [footer addSubview:line];
    [footer setBackgroundColor:[UIColor colorWithWhite:1.0 alpha:1.0]];
    self.tableView.tableHeaderView = header;
    self.tableView.tableFooterView = footer;
    
    // Do any additional setup after loading the view from its nib.
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    // Return the number of sections.
    return 3;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    // Return the number of rows in the section.
    return 1;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath  {
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
        UIView *selectionColor = [[UIView alloc] init];
        [selectionColor setBackgroundColor:[UIColor colorWithRed:95.0/255.0 green:196.0/255.0 blue:217.0/255.0 alpha:1.0]];
        [cell setSelectedBackgroundView:selectionColor];
        cell.textLabel.font = [cell.textLabel.font fontWithSize:12.0];
        cell.detailTextLabel.font = [cell.detailTextLabel.font fontWithSize:10.0];
    }
    
    return cell;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    switch (indexPath.section) {
        case 0:
            cell.textLabel.text = @"Add a keyboard for your language";
            cell.detailTextLabel.text = @"";
            if (![self didAddKeyboard])
                [cell setAccessoryType:UITableViewCellAccessoryNone];
            else
                [cell setAccessoryType:UITableViewCellAccessoryCheckmark];
            break;
        case 1:
            #ifdef FREE
                cell.textLabel.text = @"Get Keyman system-wide keyboard";
            #else
                cell.textLabel.text = @"Set up Keyman as system-wide keyboard";
            #endif
            if ([[[UIDevice currentDevice] systemVersion] intValue] >= 8) {
                cell.detailTextLabel.text = @"";
                if (![AppDelegate isKeymanEnabledSystemWide])
                    [cell setAccessoryType:UITableViewCellAccessoryNone];
                else
                    [cell setAccessoryType:UITableViewCellAccessoryCheckmark];
            }
            else {
                cell.detailTextLabel.text = @"Available on iOS 8+";
                [cell setAccessoryType:UITableViewCellAccessoryNone];
                cell.userInteractionEnabled = NO;
                cell.textLabel.enabled = NO;
                cell.detailTextLabel.enabled = NO;
            }
            break;
        case 2:
            cell.textLabel.text = @"More info";
            cell.detailTextLabel.text = @"";
            [cell setAccessoryType:UITableViewCellAccessoryDetailButton];
            break;
        default:
            break;
    }
}

// In a xib-based application, navigation from a table can be handled in -tableView:didSelectRowAtIndexPath:
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    [tableView cellForRowAtIndexPath:indexPath].selected = NO;
    [self performActionForIndexPath:indexPath];
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath {
    [self performActionForIndexPath:indexPath];
}

- (void)performActionForIndexPath:(NSIndexPath *)indexPath {
    switch (indexPath.section) {
        case 0:
            [(MainViewController *)self.mainViewController dismissGetStartedView:nil];
            [[KMManager sharedInstance] showKeyboardPickerInViewController:self.mainViewController
                                                         shouldAddKeyboard:YES];
            break;
        case 1: {
            #ifdef FREE
                [(MainViewController *)self.mainViewController dismissGetStartedView:nil];
                UpgradeViewController *upgradeVC = [[UpgradeViewController alloc] init];
                [(MainViewController *)self.mainViewController presentViewController:upgradeVC animated:YES completion:nil];
            #else
                [(MainViewController *)self.mainViewController dismissGetStartedView:nil];
                SetUpViewController *setUpVC = [[SetUpViewController alloc] init];
                [(MainViewController *)self.mainViewController presentViewController:setUpVC animated:YES completion:nil];
            #endif
            break;
        }
        case 2:
            [(MainViewController *)self.mainViewController dismissGetStartedView:nil];
            [(MainViewController *)self.mainViewController infoButtonClick:nil];
            break;
        default:
            break;
    }
}

- (void)refreshTable {
    [self.tableView reloadData];
}

- (BOOL)didAddKeyboard {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    NSArray *userKbs = [userData objectForKey:kKeymanUserKeyboardsListKey];
    if ([userKbs count] < 2) {
        NSDictionary *firstKB = [userKbs objectAtIndex:0];
        if (firstKB != nil) {
            NSString *kbID = [firstKB objectForKey:kKeymanKeyboardIdKey];
            NSString *langID = [firstKB objectForKey:kKeymanLanguageIdKey];
            if ([kbID isEqualToString:kKeymanDefaultKeyboardID] && [langID isEqualToString:langID])
                return NO;
            else
                return YES;
        }
        else
            return NO;
    }
    else
        return YES;
}

- (void)switchValueChanged:(id)sender{
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    [userData setBool:[sender isOn] forKey:kKeymanDontShowGetStartedKey];
    [userData synchronize];
}

- (BOOL)dontShowGetStarted {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    return [userData boolForKey:kKeymanDontShowGetStartedKey];
}

/*
#define DegreesToRadians(degrees) (degrees * M_PI / 180)

- (CGAffineTransform)transformForOrientation:(UIInterfaceOrientation)orientation {
    switch (orientation) {
        case UIInterfaceOrientationLandscapeLeft:
            return CGAffineTransformMakeRotation(-DegreesToRadians(90));
        case UIInterfaceOrientationLandscapeRight:
            return CGAffineTransformMakeRotation(DegreesToRadians(90));
        case UIInterfaceOrientationPortraitUpsideDown:
            return CGAffineTransformMakeRotation(DegreesToRadians(180));
        case UIInterfaceOrientationPortrait:
        default:
            return CGAffineTransformMakeRotation(DegreesToRadians(0));
    }
}
*/

@end
