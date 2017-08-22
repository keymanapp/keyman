//
//  LanguageDetailViewController.m
//  Keyman Engine
//
//  Created by Serkan Kurt on 26/07/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "LanguageDetailViewController.h"
#import "KeyboardPickerViewController.h"
#import "KeyboardNameTableViewCell.h"
#import "KMManager+Internal.h"

static const NSInteger kmToolbarButtonTag = 100;
static const NSInteger kmToolbarLabelTag = 101;
static const NSInteger kmToolbarActivityIndicatorTag = 102;

@interface LanguageDetailViewController ()
    @property (nonatomic, strong) NSMutableDictionary *userKeyboards;
    @property (assign) BOOL isUpdate;
@end

@implementation LanguageDetailViewController

@synthesize userKeyboards, languageIndex, languageName, languageID, keyboards;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)loadView {
	[super loadView];
    [self loadUserKeyboards];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadStarted:) name:kKeymanKeyboardDownloadStartedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFailed:) name:kKeymanKeyboardDownloadFailedNotification object:nil];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
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
    return [keyboards count];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 1;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[KeyboardNameTableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
        UIView *selectionColor = [[UIView alloc] init];
        [selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:136.0/255.0 blue:34.0/255.0 alpha:1.0]];
        [cell setSelectedBackgroundView:selectionColor];
    }
    
    return cell;
}

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
	//[cell setSelectionStyle:UITableViewCellSelectionStyleNone];
    [(KeyboardNameTableViewCell *)cell setIndexPath:indexPath];
    
    cell.textLabel.text = [[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanNameKey];
    NSString *keyboardID = [[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanIdKey];
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

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    [tableView cellForRowAtIndexPath:indexPath].selected = NO;
    NSString *kbID = [[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanIdKey];
    NSString *kbName = [[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanNameKey];
    //NSString *isRTL = [[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanKeyboardRTLKey];
    //isRTL = (isRTL == nil)?@"N":@"Y";
    //NSString *font = [[KMManager sharedInstance] jsFontFromFontDictionary:[[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanFontKey]];
    //NSString *oskFont = [[KMManager sharedInstance] jsFontFromFontDictionary:[[keyboards objectAtIndex:indexPath.section] objectForKey:kKeymanOskFontKey]];
    
    eKMKeyboardState state = [[KMManager sharedInstance] stateForKeyboardWithID:kbID];
    if (state != kKMKeyboardStateDownloading) {
        if (state == kKMKeyboardStateNeedsDownload)
            self.isUpdate = NO;
        else
            self.isUpdate = YES;
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"%@: %@", languageName, kbName]
                                                        message:@"Would you like to download this keyboard?"
                                                       delegate:self
                                              cancelButtonTitle:@"Cancel"
                                              otherButtonTitles:@"Download", nil];
        alert.tag = indexPath.section;
        [alert show];
    }
    /*
    if (state == kKMKeyboardStateNeedsDownload) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"%@: %@", languageName, kbName]
                                                         message:@"Would you like to download this keyboard?"
                                                        delegate:self
                                               cancelButtonTitle:@"Cancel"
                                               otherButtonTitles:@"Download", nil];
        alert.tag = indexPath.section;
        [alert show];
    }
    else if (state == kKMKeyboardStateDownloading || state == kKMKeyboardStateNone) {
        // Do nothing.
    }
    else {
        // Add keyboard.
        [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:languageID keyboardName:kbName languageName:languageName isRTL:[isRTL isEqualToString:@"Y"]?YES:NO isCustom:NO font:font oskFont:oskFont];
        [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:languageID keyboardName:kbName languageName:languageName font:font oskFont:oskFont];
        [self.navigationController popToRootViewControllerAnimated:YES];
    }*/
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {   
    // Keyboard download confirmation alert (tag is used for keyboard index).
    if (buttonIndex != alertView.cancelButtonIndex) {
        [[KMManager sharedInstance] downloadKeyboardForLanguageIndex:languageIndex keyboardIndex:alertView.tag isUpdate:self.isUpdate];
    }
}

#pragma mark - Keyman notifications

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
        [self.userKeyboards setObject:kb forKey:dictKey];
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

@end
