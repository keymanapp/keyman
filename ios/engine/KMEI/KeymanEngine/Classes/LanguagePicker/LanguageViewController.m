//
//  LanguageViewController.m
//  Keyman Engine
//
//  Created by Serkan Kurt on 26/07/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "LanguageViewController.h"
#import "LanguageDetailViewController.h"
#import "KMManager+Internal.h"
#import "QuartzCore/QuartzCore.h"
#import <UIKit/UIKit.h>
#import <KMEI-Swift.h>

static const NSInteger kmErrorAlertTag = -1;
static const NSInteger kmActivityViewTag = -2;
static const NSInteger kmToolbarButtonTag = 100;
static const NSInteger kmToolbarLabelTag = 101;
static const NSInteger kmToolbarActivityIndicatorTag = 102;

@interface LanguageViewController ()
@property (nonatomic, strong) NSMutableDictionary *userKeyboards;
@property (nonatomic, strong) NSMutableArray *sectionIndexTitles;
@property (nonatomic, strong) NSMutableArray *indices;
@property (nonatomic, assign) NSInteger selectedSection;
@property (assign) BOOL isUpdate;
@end

@implementation LanguageViewController

@synthesize userKeyboards, sectionIndexTitles, indices, selectedSection;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

#pragma mark - View lifecycle

- (void)loadView {
	[super loadView];
    self.sectionIndexTitles = nil;
    self.indices = nil;
	if ([KMManager sharedInstance].currentRequest == nil && [KMManager sharedInstance].languages == nil) {
		[KMManager fetchKeyboardsWithCompletionBlock:nil];
	}
    [self loadUserKeyboards];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.title = @"Add New Keyboard";
    self.selectedSection = NSNotFound;
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(languageFetchFinished) name:kKeymanLanguagesUpdatedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(languageFetchFailed) name:kKeymanLanguagesDownloadFailedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadStarted:) name:kKeymanKeyboardDownloadStartedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFailed:) name:kKeymanKeyboardDownloadFailedNotification object:nil];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)viewDidAppear:(BOOL)animated {
	[super viewDidAppear:animated];
	[self.navigationController setToolbarHidden:YES animated:YES];
	// if no rows to show yet, show a loading indicator
	if ([self numberOfSectionsInTableView:self.tableView] == 0) {
        [self showActivityView];
	}
}

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return [[[KMManager sharedInstance] languages] count];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 1;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *cellIdentifierType1 = @"CellType1";
    static NSString *cellIdentifierType2 = @"CellType2";
    
    NSArray *keyboards = [[KMManager sharedInstance] keyboardsForIndex:indexPath.section];
    NSString *cellIdentifier = ([keyboards count] < 2) ? cellIdentifierType1 : cellIdentifierType2;
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        if ([keyboards count] < 2) {
            cell = [[KeyboardNameTableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
            UIView *selectionColor = [[UIView alloc] init];
            [selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:136.0/255.0 blue:34.0/255.0 alpha:1.0]];
            [cell setSelectedBackgroundView:selectionColor];
        }
        else {
            cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:cellIdentifier];
            [cell setAccessoryType:UITableViewCellAccessoryDisclosureIndicator];
            UIView *selectionColor = [[UIView alloc] init];
            [selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:136.0/255.0 blue:34.0/255.0 alpha:1.0]];
            [cell setSelectedBackgroundView:selectionColor];
        }
    }
    
    cell.detailTextLabel.text = ([keyboards count] < 2) ? [[keyboards objectAtIndex:0] objectForKey:kKeymanNameKey] : @"";
    
    return cell;
}

/*
 // Override to support conditional editing of the table view.
 - (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the specified item to be editable.
 return YES;
 }
 */

/*
 // Override to support editing the table view.
 - (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
 {
 if (editingStyle == UITableViewCellEditingStyleDelete) {
 // Delete the row from the data source
 [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
 }
 else if (editingStyle == UITableViewCellEditingStyleInsert) {
 // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
 }
 }
 */

/*
 // Override to support rearranging the table view.
 - (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
 {
 }
 */

/*
 // Override to support conditional rearranging of the table view.
 - (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the item to be re-orderable.
 return YES;
 }
 */

- (NSArray *)sectionIndexTitlesForTableView:(UITableView *)tableView
{
    NSString *iosVersionStr = [[UIDevice currentDevice] systemVersion];
    if ([iosVersionStr rangeOfString:@"7.0"].location != NSNotFound)
        return nil; // return nil to prevent crash on iOS 7.0.x
    
    if (self.sectionIndexTitles == nil || ![self.sectionIndexTitles count]) {
        NSArray *languages = [KMManager sharedInstance].languages;
        self.sectionIndexTitles = [[NSMutableArray alloc] initWithCapacity:1];
        self.indices = [[NSMutableArray alloc] initWithCapacity:1];
        
        NSInteger langIndex = 0;
        for (NSDictionary *item in languages) {
            if(![self.sectionIndexTitles containsObject:[[item objectForKey:kKeymanNameKey] substringToIndex:1]]) {
                [self.sectionIndexTitles addObject:[[item objectForKey:kKeymanNameKey] substringToIndex:1]];
                [self.indices addObject:[NSNumber numberWithLong:langIndex]];
            }
            langIndex++;
        }
    }
    return self.sectionIndexTitles;
}

- (NSInteger)tableView:(UITableView *)tableView sectionForSectionIndexTitle:(NSString *)title atIndex:(NSInteger)index {
    return [[self.indices objectAtIndex:index] integerValue];
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    NSArray *languages = [[KMManager sharedInstance] languages];
    if (cell.accessoryType == UITableViewCellAccessoryDisclosureIndicator) {
        if (indexPath.section < [languages count]) {
            cell.textLabel.text = [[languages objectAtIndex:indexPath.section] objectForKey:kKeymanNameKey];
        }
    }
    else {
        if (indexPath.section < [languages count]) {
            [(KeyboardNameTableViewCell *)cell setIndexPath:indexPath];
            
            cell.textLabel.text = [[languages objectAtIndex:indexPath.section] objectForKey:kKeymanNameKey];
            NSString *languageID = [[languages objectAtIndex:indexPath.section] objectForKey:kKeymanIdKey];
            NSString *keyboardID = [[[[languages objectAtIndex:indexPath.section] objectForKey:kKeymanLanguageKeyboardsKey] objectAtIndex:0] objectForKey:kKeymanIdKey];
            if ([self isAdded:languageID keyboardID:keyboardID]) {
                [cell setAccessoryType:UITableViewCellAccessoryCheckmark];
                cell.userInteractionEnabled = NO;
                cell.textLabel.enabled = NO;
                cell.detailTextLabel.enabled = NO;
            }
            else {
                [cell setAccessoryType:UITableViewCellAccessoryNone];
                cell.userInteractionEnabled = YES;
                cell.textLabel.enabled = YES;
                cell.detailTextLabel.enabled = YES;
            }
            
            eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:keyboardID];
            [(KeyboardNameTableViewCell *)cell setKeyboardState:kbState selected:NO defaultAccessoryType:cell.accessoryType];
        }
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    selectedSection = indexPath.section;
    [tableView cellForRowAtIndexPath:indexPath].selected = NO;
	if ([tableView cellForRowAtIndexPath:indexPath].accessoryType == UITableViewCellAccessoryDisclosureIndicator) {
        [self showLanguageDetailViewWithTitle:[tableView cellForRowAtIndexPath:indexPath].textLabel.text languageIndex:indexPath.section];
    }
    else {
        NSArray *languages = [[KMManager sharedInstance] languages];
        //NSString *langID = [[languages objectAtIndex:indexPath.section] objectForKey:kKeymanIdKey];
        NSString *langName = [[languages objectAtIndex:indexPath.section] objectForKey:kKeymanNameKey];
        NSArray *keyboards = [[KMManager sharedInstance] keyboardsForIndex:indexPath.section];
        NSString *kbID = [[keyboards objectAtIndex:0] objectForKey:kKeymanIdKey];
        NSString *kbName = [[keyboards objectAtIndex:0] objectForKey:kKeymanNameKey];
        //NSString *isRTL = [[keyboards objectAtIndex:0] objectForKey:kKeymanKeyboardRTLKey];
        //isRTL = (isRTL == nil)?@"N":@"Y";
        //NSString *font = [[KMManager sharedInstance] jsFontFromFontDictionary:[[keyboards objectAtIndex:0] objectForKey:kKeymanFontKey]];
        //NSString *oskFont = [[KMManager sharedInstance] jsFontFromFontDictionary:[[keyboards objectAtIndex:0] objectForKey:kKeymanOskFontKey]];
        eKMKeyboardState state = [[KMManager sharedInstance] stateForKeyboardWithID:kbID];
        if (state != kKMKeyboardStateDownloading) {
            if (state == kKMKeyboardStateNeedsDownload)
                self.isUpdate = NO;
            else
                self.isUpdate = YES;
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"%@: %@", langName, kbName]
                                                            message:@"Would you like to download this keyboard?"
                                                           delegate:self
                                                  cancelButtonTitle:@"Cancel"
                                                  otherButtonTitles:@"Download", nil];
            alert.tag = 0;
            [alert show];
        }
        
        /*
        if (state == kKMKeyboardStateNeedsDownload) {
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"%@: %@", langName, kbName]
                                                            message:@"Would you like to download this keyboard?"
                                                           delegate:self
                                                  cancelButtonTitle:@"Cancel"
                                                  otherButtonTitles:@"Download", nil];
            alert.tag = 0;
            [alert show];
        }
        else if (state == kKMKeyboardStateDownloading || state == kKMKeyboardStateNone) {
            // Do nothing
        }
        else {
            // Add keyboard.
            [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:[isRTL isEqualToString:@"Y"]?YES:NO isCustom:NO font:font oskFont:oskFont];
            [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
            [self.navigationController popToRootViewControllerAnimated:YES];
        }*/
    }
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath {
    [self showLanguageDetailViewWithTitle:[tableView cellForRowAtIndexPath:indexPath].textLabel.text languageIndex:indexPath.section];
}

- (void)showLanguageDetailViewWithTitle:(NSString *)title languageIndex:(NSInteger)langIndex {
    LanguageDetailViewController *langDetailView = [[LanguageDetailViewController alloc] init];
    NSArray *languages = [[KMManager sharedInstance] languages];
    langDetailView.title = title;
    langDetailView.languageIndex = langIndex;
    langDetailView.languageName = [[languages objectAtIndex:langIndex] objectForKey:kKeymanNameKey];
    langDetailView.languageID = [[languages objectAtIndex:langIndex] objectForKey:kKeymanIdKey];
    langDetailView.keyboards = [[NSArray alloc] initWithArray:[[KMManager sharedInstance] keyboardsForIndex:langIndex]];

    [self.navigationController pushViewController:langDetailView animated:YES];
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
	if (alertView.tag == kmErrorAlertTag) {
		if ([KMManager sharedInstance].languages == nil) {
			[self.navigationController popToRootViewControllerAnimated:YES];
		}
	}
    else {
		// Keyboard download confirmation alert (tag is used for keyboard index).
        if (buttonIndex != alertView.cancelButtonIndex) {
            [[KMManager sharedInstance] downloadKeyboardForLanguageIndex:selectedSection keyboardIndex:alertView.tag isUpdate:self.isUpdate];
        }
	}
}

#pragma mark - Keyman notifications

- (void)languageFetchFinished {
    [self dismissActivityView];
    [self.tableView reloadData];
    if ([self numberOfSectionsInTableView:self.tableView] == 0) {
        [self showConnectionErrorAlert];
	}
}

- (void)languageFetchFailed {
    [self dismissActivityView];
    [self showConnectionErrorAlert];
}

- (void)keyboardDownloadStarted:(NSNotification *)notification {
    [self.view setUserInteractionEnabled:NO];
    [self.navigationItem setHidesBackButton:YES animated:YES];
    
    CGRect frame = [self.navigationController.toolbar frame];
    CGFloat width = frame.size.width;
    CGFloat height = frame.size.height;
    frame.size.width = width*0.95;
    frame.size.height = height*0.7;
    UILabel *label = [[UILabel alloc] initWithFrame:frame];
    [label setBackgroundColor:[UIColor clearColor]];
    [label setTextColor:[UIColor whiteColor]];
    [label setTextAlignment:NSTextAlignmentCenter];
    [label setCenter:CGPointMake(width*0.5, height*0.5)];
    [label setText:@"Downloading\u2026"];
    [label setAutoresizingMask:UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin | UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight];
    [label setTag:kmToolbarLabelTag];
    
    UIActivityIndicatorView *indicatorView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleGray];
    frame = [indicatorView frame];
    [indicatorView setCenter:CGPointMake(width - frame.size.width, height*0.5)];
    [indicatorView setAutoresizingMask:UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin];
    [indicatorView setTag:kmToolbarActivityIndicatorTag];
    [indicatorView startAnimating];
    
    [[self.navigationController.toolbar viewWithTag:kmToolbarButtonTag] removeFromSuperview];
    [[self.navigationController.toolbar viewWithTag:kmToolbarLabelTag] removeFromSuperview];
    [[self.navigationController.toolbar viewWithTag:kmToolbarActivityIndicatorTag] removeFromSuperview];
    [self.navigationController.toolbar addSubview:label];
    [self.navigationController.toolbar addSubview:indicatorView];
    [self.navigationController setToolbarHidden:NO animated:YES];
}

- (void)keyboardDownloadFailed:(NSNotification *)notification {
    [self.view setUserInteractionEnabled:YES];
    [self.navigationItem setHidesBackButton:NO animated:YES];
}

#pragma mark - Private methods

- (void)showActivityView {
    [self.view setUserInteractionEnabled:NO];
    
    UIActivityIndicatorView *indicatorView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleWhiteLarge];
    UIView *activityView = [[UIView alloc] initWithFrame:CGRectInset(indicatorView.bounds, -10.0f, -10.0f)];
    activityView.backgroundColor = [UIColor colorWithWhite:0.5f alpha:0.8f];
    activityView.layer.cornerRadius = 6.0f;
    activityView.center = self.view.center;
    activityView.tag = kmActivityViewTag;
    activityView.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin;
    
    indicatorView.center = CGPointMake(activityView.bounds.size.width*0.5f, activityView.bounds.size.height*0.5f);
    [indicatorView startAnimating];
    [activityView addSubview:indicatorView];
    
    [self.view addSubview:activityView];
}

- (void)dismissActivityView {
    UIView *activityView = [self.view viewWithTag:kmActivityViewTag];
    [activityView removeFromSuperview];
    [self.view setUserInteractionEnabled:YES];
}

- (void)loadUserKeyboards {
    NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
    NSArray *userKbList = [[NSArray alloc] initWithArray:[userData arrayForKey:kKeymanUserKeyboardsListKey]];
    NSUInteger len = [userKbList count];
    
    if (len == 0) {
        userKeyboards = nil;
        return;
    }
    
    NSString *langID = nil;
    NSString *kbID = nil;
    NSString *dictKey = nil;
    userKeyboards = [[NSMutableDictionary alloc] initWithCapacity:len];
    for (NSDictionary *kb in userKbList) {
        langID = [kb objectForKey:kKeymanLanguageIdKey];
        kbID = [kb objectForKey:kKeymanKeyboardIdKey];
        dictKey = [NSString stringWithFormat:@"%@_%@", langID, kbID];
        [userKeyboards setObject:kb forKey:dictKey];
    }
}

- (BOOL)isAdded:(NSString *)langID keyboardID:(NSString *)kbID {
    if (userKeyboards != nil) {
        if([userKeyboards objectForKey:[NSString stringWithFormat:@"%@_%@", langID, kbID]]) {
            return YES;
        }
        else {
            return NO;
        }
    }
    else {
        return NO;
    }
}

- (void)showConnectionErrorAlert {
    [self dismissActivityView];
    
	UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Connection Error"
                                                    message:@"Could not reach Keyman server. Please try again later."
                                                   delegate:self
                                          cancelButtonTitle:@"OK"
                                          otherButtonTitles:nil];
	alert.tag = kmErrorAlertTag;
	[alert show];
}

@end
