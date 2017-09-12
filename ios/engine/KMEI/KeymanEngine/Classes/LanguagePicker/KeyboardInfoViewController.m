//
//  KeyboardInfoViewController.m
//  KeymanEngine
//
//  Created by Serkan Kurt on 3/06/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardInfoViewController.h"
#import "KMManager+Internal.h"

#define KMBackgroundQueue dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)

@interface KeyboardInfoViewController ()
    @property (nonatomic, strong) NSMutableArray *infoArray;
    @property (nonatomic, strong) NSDictionary *kbDict;
@end

@implementation KeyboardInfoViewController
@synthesize keyboardCount, keyboardIndex;
@synthesize keyboardID, languageID, keyboardVersion, keyboardCopyright;
@synthesize infoArray, isCustomKeyboard;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    // Copyright info is disabled
    /*
    NSString *kbCopyright;
    NSString *deviceType = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?@"iphone":@"ipad";
    NSString *urlStr = [NSString stringWithFormat:@"%@languages/%@/%@?device=%@", kKeymanApiBaseURL, languageID, keyboardID, deviceType];
    
    if (keyboardCopyright == nil || ![keyboardCopyright length]) {
        kbCopyright = @"Unknown";
        dispatch_async(KMBackgroundQueue, ^{
            NSData *data = [NSData dataWithContentsOfURL:[NSURL URLWithString:urlStr]];
            [self performSelectorOnMainThread:@selector(fetchedKeyboardData:)
                                   withObject:data waitUntilDone:YES];
        });
    }
    else
        kbCopyright = keyboardCopyright;
    */
    
    infoArray = [[NSMutableArray alloc] initWithCapacity:3];
    [infoArray addObject:[NSMutableDictionary dictionaryWithObjectsAndKeys:@"Keyboard version", @"title", keyboardVersion, @"subtitle", nil]];
    //[infoArray addObject:[NSMutableDictionary dictionaryWithObjectsAndKeys:@"Copyright", @"title", kbCopyright, @"subtitle", nil]];
    if (!isCustomKeyboard) {
        [infoArray addObject:[NSMutableDictionary dictionaryWithObjectsAndKeys:@"Help link", @"title", @"", @"subtitle", nil]];
    }
    [infoArray addObject:[NSMutableDictionary dictionaryWithObjectsAndKeys:@"Delete", @"title", @"", @"subtitle", nil]];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)viewDidAppear:(BOOL)animated {
	[super viewDidAppear:animated];
	[self.navigationController setToolbarHidden:YES animated:YES];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (isCustomKeyboard)
        return 2;
    else
        return 3;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
    }
    
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (!isCustomKeyboard) {
        if( indexPath.row == 1) {
            NSString *helpUrlStr = [NSString stringWithFormat:@"http://help.keyman.com/keyboard/%@/%@/", keyboardID, keyboardVersion];
            [[UIApplication sharedApplication] openURL:[NSURL URLWithString:helpUrlStr]];
        }
        else if (indexPath.row == 2) {
            [self showDeleteKeyboard];
        }
    }
    else if (indexPath.row == 1) {
        [self showDeleteKeyboard];
    }
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
    } else if (editingStyle == UITableViewCellEditingStyleInsert) {
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

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

- (void)fetchedKeyboardData:(NSData *)data {
    NSError *error;
    NSDictionary *json = [NSJSONSerialization
                          JSONObjectWithData:data
                          options:0
                          error:&error];
    if (error == nil) {
        NSArray *keyboards = [[json objectForKey:kKeymanLanguageKey] objectForKey:kKeymanLanguageKeyboardsKey];
        NSDictionary *kbDict = [keyboards objectAtIndex:0];
        NSMutableDictionary *info = [infoArray objectAtIndex:1];
        NSString *copyright = [kbDict objectForKey:kKeymanKeyboardCopyrightKey];
        if (copyright == nil)
            copyright = @"Unknown";
        
        [info setObject:copyright forKey:@"subtitle"];
        [infoArray replaceObjectAtIndex:1 withObject:info];
        [self.tableView reloadData];
    }
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    [cell setSelectionStyle:UITableViewCellSelectionStyleNone];
    [cell setAccessoryType:UITableViewCellAccessoryNone];
    cell.textLabel.text = [[infoArray objectAtIndex:indexPath.row] objectForKey:@"title"];
    cell.detailTextLabel.text = [[infoArray objectAtIndex:indexPath.row] objectForKey:@"subtitle"];
    cell.tag = indexPath.row;
    if (!isCustomKeyboard) {
        if (indexPath.row == 1)
            [cell setAccessoryType:UITableViewCellAccessoryDisclosureIndicator];
        else if (indexPath.row == 2 && ![self canDeleteKeyboard]) {
            cell.userInteractionEnabled = NO;
            cell.textLabel.enabled = NO;
            cell.detailTextLabel.enabled = NO;
        }
    }
    else if (indexPath.row == 1 && ![self canDeleteKeyboard]) {
        cell.userInteractionEnabled = NO;
        cell.textLabel.enabled = NO;
        cell.detailTextLabel.enabled = NO;
    }
}

- (BOOL)isCurrentKeyboard {
    if ([[KMManager sharedInstance].keyboardID isEqualToString:keyboardID] && [[KMManager sharedInstance].languageID isEqualToString:languageID]) {
        return YES;
    }
    else {
        return NO;
    }
}

- (BOOL)canDeleteKeyboard {
    if (![[KMManager sharedInstance] canRemoveKeyboards])
        return NO;
    
    if (![[KMManager sharedInstance] canRemoveDefaultKeyboard]) {
        if (keyboardIndex == 0)
            return NO;
        else
            return YES;
    }
    else {
        if (keyboardIndex > 0)
            return YES;
        else if (keyboardCount > 1)
            return YES;
        else
            return NO;
    }
}

- (void)showDeleteKeyboard {
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:self.title
                                                    message:@"Would you like to delete this keyboard?"
                                                   delegate:self
                                          cancelButtonTitle:@"Cancel"
                                          otherButtonTitles:@"Delete", nil];
    alert.tag = 1;
    [alert show];
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (buttonIndex == alertView.cancelButtonIndex)
        return;
    
    if (alertView.tag == 1) {
        NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
        NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
        NSDictionary *kbDict = [NSDictionary dictionaryWithDictionary:[userKeyboards objectAtIndex:keyboardIndex]];
        
        if ([[KMManager sharedInstance] removeKeyboardAtIndex:keyboardIndex]) {
            if ([self isCurrentKeyboard]) {
                NSString *kbID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
                NSString *langID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
                NSString *kbName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardNameKey];
                NSString *langName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageNameKey];
                NSString *font = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanFontKey];
                NSString *oskFont = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanOskFontKey];
                [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
            }
            
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardRemovedNotification
                                                                object:self
                                                              userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbDict, kKeymanKeyboardInfoKey, nil]];
            [self.navigationController popToRootViewControllerAnimated:YES];
        }
    }
}

@end
