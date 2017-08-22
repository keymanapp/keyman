//
//  KeyboardPickerViewController.m
//  Keyman Engine
//
//  Created by Serkan Kurt on 25/07/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardPickerViewController.h"
#import "LanguageViewController.h"
#import "KeyboardInfoViewController.h"
#import "KMManager+Internal.h"

static const NSInteger kmErrorAlertTag = -1;
static const NSInteger kmToolbarButtonTag = 100;
static const NSInteger kmToolbarLabelTag = 101;
static const NSInteger kmToolbarActivityIndicatorTag = 102;

@interface KeyboardPickerViewController ()
@property (nonatomic, strong) NSMutableArray *userKeyboards;
@property (nonatomic, strong) NSMutableArray *updateQueue;
@property (nonatomic, assign) BOOL isDoneButtonEnabled;
@property (nonatomic, assign) BOOL didUpdateCheck;
@end

@implementation KeyboardPickerViewController

@synthesize userKeyboards, updateQueue, isDoneButtonEnabled, didUpdateCheck;

- (id)initWithStyle:(UITableViewStyle)style {
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    
    self.title = @"Keyboards";
    [self setIsDoneButtonEnabled:NO];
    didUpdateCheck = NO;
    updateQueue = nil;
    if ([[KMManager sharedInstance] canAddNewKeyboards]) {
        UIBarButtonItem *addButton = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemAdd target:self action:@selector(addClicked:)];
        self.navigationItem.rightBarButtonItem = addButton;
    }
    
    if ([self.navigationController.toolbar respondsToSelector:@selector(setBarTintColor:)])
        [self.navigationController.toolbar setBarTintColor:[UIColor colorWithRed:0.5 green:0.75 blue:0.25 alpha:0.9]];
    else
        [self.navigationController.toolbar setTintColor:[UIColor colorWithRed:0.5 green:0.75 blue:0.25 alpha:0.9]];
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadStarted:) name:kKeymanKeyboardDownloadStartedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFinished:) name:kKeymanKeyboardDownloadCompletedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFailed:) name:kKeymanKeyboardDownloadFailedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    
    [self loadUserKeyboards];
    [self scrollToSelectedKeyboard:NO];
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    if (!didUpdateCheck) {
        if ([self checkUpdates]) {
            CGRect frame = [self.navigationController.toolbar frame];
            CGFloat width = frame.size.width;
            CGFloat height = frame.size.height;
            UIButton *button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
            [button addTarget:self action:@selector(updateClicked:) forControlEvents:UIControlEventTouchUpInside];
            frame.size.width = width*0.95;
            frame.size.height = height*0.7;
            [button setFrame:frame];
            [button setCenter:CGPointMake(width/2, height/2)];
            [button setTintColor:[UIColor colorWithRed:0.75 green:1.0 blue:0.5 alpha:1.0]];
            if ([[[UIDevice currentDevice] systemVersion] intValue] >= 7)
                [button setTitleColor:[UIColor whiteColor] forState:UIControlStateNormal];
            [button setTitle:@"Update available" forState:UIControlStateNormal];
            [button setAutoresizingMask:UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin | UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight];
            [button setTag:kmToolbarButtonTag];
            [self.navigationController.toolbar addSubview:button];
            
            [self.navigationController setToolbarHidden:NO animated:YES];
            [self scrollToSelectedKeyboard:NO];
        }
    }
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [userKeyboards count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
        UIView *selectionColor = [[UIView alloc] init];
        [selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:136.0/255.0 blue:34.0/255.0 alpha:1.0]];
        //[selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:95.0/255.0 blue:60.0/255.0 alpha:1.0]];
        //[selectionColor setBackgroundColor:[UIColor colorWithRed:255.0/255.0 green:137.0/255.0 blue:51.0/255.0 alpha:1.0]];
        //[selectionColor setBackgroundColor:[UIColor colorWithRed:255.0/255.0 green:147.0/255.0 blue:109.0/255.0 alpha:1.0]];
        //[selectionColor setBackgroundColor:[UIColor colorWithRed:95.0/255.0 green:196.0/255.0 blue:217.0/255.0 alpha:1.0]];
        //[selectionColor setBackgroundColor:[UIColor colorWithRed:213.0/255.0 green:134.0/255.0 blue:64.0/255.0 alpha:1.0]];
        [cell setSelectedBackgroundView:selectionColor];
    }
    
    return cell;
}

// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the specified item to be editable.
    if (![[KMManager sharedInstance] canRemoveKeyboards])
        return NO;
    
    if (![[KMManager sharedInstance] canRemoveDefaultKeyboard]) {
        if (indexPath.row == 0)
            return NO;
        else
            return YES;
    }
    else {
        if (indexPath.row > 0)
            return YES;
        else if ([userKeyboards count] > 1)
            return YES;
        else
            return NO;
    }
}
/*
- (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath {
    return YES;
}

- (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)sourceIndexPath toIndexPath:(NSIndexPath *)destinationIndexPath {
    NSString *stringToMove = [self.userKeyboards objectAtIndex:sourceIndexPath.row];
    [self.userKeyboards removeObjectAtIndex:sourceIndexPath.row];
    [self.userKeyboards insertObject:stringToMove atIndex:destinationIndexPath.row];
}*/

// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        // Delete the row from the data source
        //[tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
        
        NSDictionary *kbDict = [NSDictionary dictionaryWithDictionary:[userKeyboards objectAtIndex:indexPath.row]];
        NSString *selKbID = [kbDict objectForKey:kKeymanKeyboardIdKey];
        NSString *selLangID = [kbDict objectForKey:kKeymanLanguageIdKey];
        BOOL isCurrentKB = [self isCurrentKeyboard:selLangID keyboardID:selKbID];
        if ([[KMManager sharedInstance] removeKeyboardAtIndex:indexPath.row]) {
            NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
            userKeyboards = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
            
            if (isCurrentKB) {
                NSString *kbID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
                NSString *langID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
                NSString *kbName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardNameKey];
                NSString *langName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageNameKey];
                NSString *font = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanFontKey];
                NSString *oskFont = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanOskFontKey];
                [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
            }
            
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardRemovedNotification object:self
                                                              userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbDict, kKeymanKeyboardInfoKey, nil]];
            [self.tableView reloadData];
        }
        [self setIsDoneButtonEnabled:YES];
    }
    else if (editingStyle == UITableViewCellEditingStyleInsert) {
        // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
    }
}

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

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath {
    [self showKeyboardInfoViewWithIndex:indexPath.row];
}

- (void)showKeyboardInfoViewWithIndex:(NSInteger)index {
    [self setIsDoneButtonEnabled:YES];
    NSString *kbID = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanKeyboardIdKey];
    NSString *langID = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanLanguageIdKey];
    NSString *kbName = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanKeyboardNameKey];
    NSString *kbVersion = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanKeyboardVersionKey];
    NSString *isCustom = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanCustomKeyboardKey];
    if (isCustom == nil)
        isCustom = @"N";
    
    if(kbVersion == nil) {
        kbVersion = [[KMManager sharedInstance] latestKeyboardFileVersionWithID:kbID];
        if (kbVersion == nil)
            kbVersion = @"1.0";
    }
    
    KeyboardInfoViewController *infoView = [[KeyboardInfoViewController alloc] init];
    infoView.title = kbName;
    infoView.keyboardCount = [userKeyboards count];
    infoView.keyboardIndex = index;
    infoView.keyboardID = kbID;
    infoView.languageID = langID;
    infoView.keyboardVersion = kbVersion;
    infoView.isCustomKeyboard = [isCustom isEqualToString:@"Y"]?YES:NO;
    [self.navigationController pushViewController:infoView animated:YES];
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    [cell setSelectionStyle:UITableViewCellSelectionStyleNone];
    NSString *languageID = [[userKeyboards objectAtIndex:indexPath.row] objectForKey:kKeymanLanguageIdKey];
    NSString *keyboardID = [[userKeyboards objectAtIndex:indexPath.row] objectForKey:kKeymanKeyboardIdKey];
    cell.textLabel.text = [[userKeyboards objectAtIndex:indexPath.row] objectForKey:kKeymanLanguageNameKey];
    cell.detailTextLabel.text = [[userKeyboards objectAtIndex:indexPath.row] objectForKey:kKeymanKeyboardNameKey];
    cell.tag = indexPath.row;
    if ([[KMManager sharedInstance].languageID isEqualToString:languageID] && [[KMManager sharedInstance].keyboardID isEqualToString:keyboardID]) {
        [cell setSelectionStyle:UITableViewCellSelectionStyleBlue];
        [cell setSelected:YES];
        [cell setAccessoryType:UITableViewCellAccessoryDetailDisclosureButton];
    }
    else {
        [cell setSelectionStyle:UITableViewCellSelectionStyleNone];
        [cell setSelected:NO];
        [cell setAccessoryType:UITableViewCellAccessoryDetailDisclosureButton];
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    //selectedRow = indexPath.row;
    [self switchKeyboard:indexPath.row];
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (alertView.tag == kmErrorAlertTag) {
        // Do nothing.
    }
}

#pragma mark - Keyman notifications

- (void)keyboardDownloadStarted:(NSNotification *)notification {
    [self.view setUserInteractionEnabled:NO];
    [self.navigationItem leftBarButtonItem].enabled = NO;
    if ([self.navigationItem rightBarButtonItem] != nil)
        [self.navigationItem rightBarButtonItem].enabled = NO;
}

- (void)keyboardDownloadFinished:(NSNotification *)notification {
    if([self.view isEqual:self.navigationController.topViewController.view]) {
        if (updateQueue) {
            [[KMManager sharedInstance] setShouldReloadKeyboard:YES];
            
            // Update keyboard version
            NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
            NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
            NSString *kbVersion = [[[[KMManager sharedInstance] keyboardsInfo] objectForKey:kbID] objectForKey:kKeymanKeyboardVersionKey];
            if (kbVersion != nil) {
                [[KMManager sharedInstance] updateKeyboardVersionForID:kbID newKeyboardVersion:kbVersion];
            }
            
            [updateQueue removeObjectAtIndex:0];
            if ([updateQueue count] > 0) {
                NSString *langID = [[updateQueue objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
                NSString *kbID = [[updateQueue objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
                [[KMManager sharedInstance] downloadKeyboardWithID:kbID languageID:langID isUpdate:YES];
            }
            else {
                [self loadUserKeyboards];
                [self.view setUserInteractionEnabled:YES];
                [self.navigationItem leftBarButtonItem].enabled = YES;
                if ([self.navigationItem rightBarButtonItem] != nil)
                    [self.navigationItem rightBarButtonItem].enabled = YES;
                updateQueue = nil;
                
                UILabel *label = (UILabel *)[self.navigationController.toolbar viewWithTag:kmToolbarLabelTag];
                [label setText:@"Keyboards successfully updated!"];
                [[self.navigationController.toolbar viewWithTag:kmToolbarActivityIndicatorTag] removeFromSuperview];
                [NSTimer scheduledTimerWithTimeInterval:3.0 target:self selector:@selector(hideToolbarDelayed:) userInfo:nil repeats:NO];
            }
        }
    }
    else {
        UILabel *label = (UILabel *)[self.navigationController.toolbar viewWithTag:kmToolbarLabelTag];
        [label setText:@"Keyboard successfully downloaded!"];
        [[self.navigationController.toolbar viewWithTag:kmToolbarActivityIndicatorTag] removeFromSuperview];
        [NSTimer scheduledTimerWithTimeInterval:3.0 target:self selector:@selector(hideToolbarDelayed:) userInfo:nil repeats:NO];
        
        [self.view setUserInteractionEnabled:YES];
        [self.navigationItem leftBarButtonItem].enabled = YES;
        if ([self.navigationItem rightBarButtonItem] != nil)
            [self.navigationItem rightBarButtonItem].enabled = YES;
        
        NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
        NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
        NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
        NSString *key = [NSString stringWithFormat:@"%@_%@", langID, kbID];
        NSDictionary *keyboardDict = [[[KMManager sharedInstance] keyboardsDictionary] objectForKey:key];
        NSString *langName = [keyboardDict objectForKey:kKeymanLanguageNameKey];
        NSString *kbName = [keyboardDict objectForKey:kKeymanKeyboardNameKey];
        BOOL isRTL = [[keyboardDict objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
        NSString *font = [keyboardDict objectForKey:kKeymanFontKey];
        NSString *oskFont = [keyboardDict objectForKey:kKeymanOskFontKey];
        
        // Add keyboard.
        [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:NO font:font oskFont:oskFont];
        [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
        [self.navigationController popToRootViewControllerAnimated:YES];
    }
}

- (void)keyboardDownloadFailed:(NSNotification *)notification {
    [self.view setUserInteractionEnabled:YES];
    [self.navigationItem leftBarButtonItem].enabled = YES;
    if ([self.navigationItem rightBarButtonItem] != nil)
        [self.navigationItem rightBarButtonItem].enabled = YES;
    
    if([self.view isEqual:self.navigationController.topViewController.view]) {
        updateQueue = nil;
        [self.navigationController setToolbarHidden:YES animated:YES];
        
        NSError *error = [[notification userInfo] objectForKey:NSUnderlyingErrorKey];
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"Keyboard Update Error"]
                                                        message:error.localizedDescription
                                                       delegate:self
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        alert.tag = kmErrorAlertTag;
        [alert show];
    }
    else {
        [self.navigationController setToolbarHidden:YES animated:YES];
        
        NSError *error = [[notification userInfo] objectForKey:NSUnderlyingErrorKey];
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"Keyboard Download Error"]
                                                        message:error.localizedDescription
                                                       delegate:self
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        alert.tag = kmErrorAlertTag;
        [alert show];
    }
}

- (void)keyboardChanged:(NSNotification *)notification {
    /*
     NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
     NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
     NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
     NSString *kbName, *langName, *kbVersion, *isRTL, *font;
     
     NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
     NSInteger index = [[KMManager sharedInstance] indexForUserKeyboardWithID:kbID languageID:langID];
     if (index >= 0) {
     NSArray *keyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
     kbName = [[keyboards objectAtIndex:index] objectForKey:kKeymanKeyboardNameKey];
     langName = [[keyboards objectAtIndex:index] objectForKey:kKeymanLanguageNameKey];
     kbVersion = [[keyboards objectAtIndex:index] objectForKey:kKeymanKeyboardVersionKey];
     isRTL = [[keyboards objectAtIndex:index] objectForKey:kKeymanKeyboardRTLKey];
     font = [[keyboards objectAtIndex:index] objectForKey:kKeymanFontKey];
     }
     else {
     NSString *key = [NSString stringWithFormat:@"%@_%@", langID, kbID];
     NSDictionary *keyboardDict = [[[KMManager sharedInstance] keyboardsDictionary] objectForKey:key];
     kbName = [keyboardDict objectForKey:kKeymanKeyboardNameKey];
     langName = [keyboardDict objectForKey:kKeymanLanguageNameKey];
     kbVersion = [keyboardDict objectForKey:kKeymanKeyboardVersionKey];
     isRTL = [keyboardDict objectForKey:kKeymanKeyboardRTLKey];
     font = [keyboardDict objectForKey:kKeymanFontKey];
     }
     
     [userData setObject:[NSDictionary dictionaryWithObjectsAndKeys:
     kbID, kKeymanKeyboardIdKey,
     langID, kKeymanLanguageIdKey,
     kbName, kKeymanKeyboardNameKey,
     langName, kKeymanLanguageNameKey,
     (kbVersion != nil)?kbVersion:@"1.0", kKeymanKeyboardVersionKey,
     (isRTL != nil)?isRTL:@"N", kKeymanKeyboardRTLKey,
     font, kKeymanFontKey,
     nil] forKey:kKeymanUserCurrentKeyboardKey];
     [userData synchronize];*/
}

#pragma mark - Private methods

- (void)switchKeyboard:(NSInteger)index {
    // Switch keyboard and register to user defaults.
    NSString *langID = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanLanguageIdKey];
    NSString *kbID = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanKeyboardIdKey];
    NSString *langName = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanLanguageNameKey];
    NSString *kbName = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanKeyboardNameKey];
    NSString *font = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanFontKey];
    NSString *oskFont = [[userKeyboards objectAtIndex:index] objectForKey:kKeymanOskFontKey];
    
    if([[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont])
        [self.tableView reloadData];
    
    if (!isDoneButtonEnabled)
        [[KMManager sharedInstance] dismissKeyboardPicker:self];
}

- (void)loadUserKeyboards {
    NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
    userKeyboards = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
    BOOL shouldSynchronize = NO;
    
    if (userKeyboards == nil) {
        NSString *kbVersion = [[KMManager sharedInstance] latestKeyboardFileVersionWithID:kKeymanDefaultKeyboardID];
        userKeyboards = [[NSMutableArray alloc] initWithObjects:[NSDictionary dictionaryWithObjectsAndKeys:
                                                                 kKeymanDefaultKeyboardID, kKeymanKeyboardIdKey,
                                                                 kKeymanDefaultLanguageID, kKeymanLanguageIdKey,
                                                                 kKeymanDefaultKeyboardName, kKeymanKeyboardNameKey,
                                                                 kKeymanDefaultLanguageName, kKeymanLanguageNameKey,
                                                                 kbVersion, kKeymanKeyboardVersionKey,
                                                                 kKeymanDefaultKeyboardRTL, kKeymanKeyboardRTLKey,
                                                                 kKeymanDefaultKeyboardFont, kKeymanFontKey,
                                                                 nil], nil];
        
        shouldSynchronize = YES;
    }
    /* no longer needed
    else {
        NSMutableDictionary *kb = [[userKeyboards objectAtIndex:0] mutableCopy];
        if ([[kb objectForKey:kKeymanKeyboardIdKey] isEqualToString:@"us"]) {
            NSString *kbVersion = [[KMManager sharedInstance] latestKeyboardFileVersionWithID:kKeymanDefaultKeyboardID];
            [kb setObject:kKeymanDefaultKeyboardID forKey:kKeymanKeyboardIdKey];
            [kb setObject:kKeymanDefaultLanguageID forKey:kKeymanLanguageIdKey];
            [kb setObject:kKeymanDefaultKeyboardName forKey:kKeymanKeyboardNameKey];
            [kb setObject:kKeymanDefaultLanguageName forKey:kKeymanLanguageNameKey];
            [kb setObject:kbVersion forKey:kKeymanKeyboardVersionKey];
            [kb setObject:kKeymanDefaultKeyboardFont forKey:kKeymanFontKey];
            
            [userKeyboards replaceObjectAtIndex:0 withObject:kb];
            shouldSynchronize = YES;
        }
        
        int index2Remove = -1;
        NSUInteger len = userKeyboards.count;
        for (int i = 0; i < len; i++) {
            kb = [[userKeyboards objectAtIndex:i] mutableCopy];
            if ([[kb objectForKey:kKeymanKeyboardIdKey] isEqualToString:kKeymanDefaultKeyboardID]) {
                if ([[kb objectForKey:kKeymanLanguageIdKey] isEqualToString:kKeymanDefaultLanguageID]) {
                    if (i > 0)
                        index2Remove = i;
                }
                else {
                    NSString *version = [kb objectForKey:kKeymanKeyboardVersionKey];
                    NSString *latestVersion = [[KMManager sharedInstance] latestKeyboardFileVersionWithID:kKeymanDefaultKeyboardID];
                    if (version == nil || ![version isEqualToString:latestVersion]) {
                        [kb setObject:latestVersion forKey:kKeymanKeyboardVersionKey];
                        shouldSynchronize = YES;
                    }
                }
            }
        }
        
        if (index2Remove > 0) {
            [userKeyboards removeObjectAtIndex:index2Remove];
            shouldSynchronize = YES;
        }
    }*/
    
    if (shouldSynchronize) {
        [userData setObject:userKeyboards forKey:kKeymanUserKeyboardsListKey];
        [userData synchronize];
    }

    [self.tableView reloadData];
}

- (BOOL)isAdded:(NSString *)langID keyboardID:(NSString *)kbID {
    if (userKeyboards != nil) {
        NSString *languageID = nil;
        NSString *keyboardID = nil;
        
        BOOL found = NO;
        for (NSDictionary *kb in userKeyboards) {
            languageID = [kb objectForKey:kKeymanLanguageIdKey];
            keyboardID = [kb objectForKey:kKeymanKeyboardIdKey];
            if([languageID isEqualToString:langID] && [keyboardID isEqualToString:kbID]) {
                found = YES;
                break;
            }
        }
        
        return found;
    }
    else {
        return NO;
    }
}

- (void)doneClicked:(id)sender {
    [[KMManager sharedInstance] dismissKeyboardPicker:self];
}

- (void)cancelClicked:(id)sender {
    [[KMManager sharedInstance] dismissKeyboardPicker:self];
}

- (void)addClicked:(id)sender {
    [self showAddKeyboard];
}

- (void)updateClicked:(id)sender {
    [[self.navigationController.toolbar viewWithTag:kmToolbarButtonTag] removeFromSuperview];
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
    [label setText:@"Updating\u2026"];
    [label setAutoresizingMask:UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin | UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight];
    [label setTag:kmToolbarLabelTag];
    
    UIActivityIndicatorView *indicatorView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleGray];
    frame = [indicatorView frame];
    [indicatorView setCenter:CGPointMake(width - frame.size.width, height*0.5)];
    [indicatorView setAutoresizingMask:UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin];
    [indicatorView setTag:kmToolbarActivityIndicatorTag];
    [indicatorView startAnimating];
    
    [self.navigationController.toolbar addSubview:label];
    [self.navigationController.toolbar addSubview:indicatorView];
    [self setIsDoneButtonEnabled:YES];
    [self updateKeyboards];
}

- (BOOL)checkUpdates {
    if ([[KMManager sharedInstance] keyboardsInfo] == nil)
        return NO;
    
    BOOL update = NO;
    NSString *kbID = nil;
    for (NSDictionary *kb in userKeyboards) {
        kbID = [kb objectForKey:kKeymanKeyboardIdKey];
        eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:kbID];
        if (kbState == kKMKeyboardStateNeedsUpdate) {
            update = YES;
            break;
        }
    }
    
    didUpdateCheck = YES;
    return update;
}

- (void)updateKeyboards {
    NSString *kbID = nil;
    NSMutableArray *kbIDs = [NSMutableArray arrayWithCapacity:[userKeyboards count]];
    updateQueue = [[NSMutableArray alloc] initWithCapacity:[userKeyboards count]];
    for (NSDictionary *kb in userKeyboards) {
        kbID = [kb objectForKey:kKeymanKeyboardIdKey];
        eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:kbID];
        if (kbState == kKMKeyboardStateNeedsUpdate) {
            if (![kbIDs containsObject:kbID]) {
                [kbIDs addObject:kbID];
                [updateQueue addObject:kb];
            }
        }
    }
    
    if ([updateQueue count] > 0) {
        NSString *langID = [[updateQueue objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
        NSString *kbID = [[updateQueue objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
        [[KMManager sharedInstance] downloadKeyboardWithID:kbID languageID:langID isUpdate:YES];
    }
}

- (void)scrollToSelectedKeyboard:(BOOL)animated {
    NSString *langID = nil;
    NSString *kbID = nil;
    int index = 0;
    for (NSDictionary *kb in userKeyboards) {
        langID = [kb objectForKey:kKeymanLanguageIdKey];
        kbID = [kb objectForKey:kKeymanKeyboardIdKey];
        if ([[KMManager sharedInstance].languageID isEqualToString:langID] && [[KMManager sharedInstance].keyboardID isEqualToString:kbID]) {
            break;
        }
        else {
            index++;
        }
    }
    
    if (index < [self.tableView numberOfRowsInSection:0]) {
        NSIndexPath *indexPath = [NSIndexPath indexPathForRow:index inSection:0];
        [self.tableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionMiddle animated:animated];
    }
}

- (void)setIsDoneButtonEnabled:(BOOL)value {
    isDoneButtonEnabled = value;
    if (isDoneButtonEnabled) {
        UIBarButtonItem *doneButton = [[UIBarButtonItem alloc] initWithTitle:@"Done" style:UIBarButtonItemStylePlain target:self action:@selector(doneClicked:)];
        self.navigationItem.leftBarButtonItem = doneButton;
    }
    else {
        UIBarButtonItem *cancelButton = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemCancel target:self action:@selector(cancelClicked:)];
        self.navigationItem.leftBarButtonItem = cancelButton;
    }
}

- (BOOL)isCurrentKeyboard:(NSString *)langID keyboardID:(NSString *)kbID {
    if ([[KMManager sharedInstance].keyboardID isEqualToString:kbID] && [[KMManager sharedInstance].languageID isEqualToString:langID]) {
        return YES;
    }
    else {
        return NO;
    }
}

- (void)hideToolbarDelayed:(NSTimer *)timer {
    [self.navigationController setToolbarHidden:YES animated:YES];
}

- (void)showAddKeyboard {
    UIButton *button = (UIButton *)[self.navigationController.toolbar viewWithTag:kmToolbarButtonTag];
    [button setEnabled:NO];
    LanguageViewController *vc = [[LanguageViewController alloc] init];
    [self.navigationController pushViewController:vc animated:YES];
    [self setIsDoneButtonEnabled:YES];
}

@end
