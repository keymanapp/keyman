//
//  KMBookmarksViewController.m
//  Keyman
//
//  Created by Serkan Kurt on 19/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMBookmarksViewController.h"
#import "AppDelegate.h"

// Bookmark keys
NSString *const kKMWebBrowserBookmarksKey = @"KMWebBrowserBookmarks";
NSString *const kBookmarkTitleKey = @"BookmarkTitle";
NSString *const kBookmarkUrlKey = @"BookMarkUrl";

@interface KMBookmarksViewController ()

@property (strong, nonatomic) IBOutlet UITableView *tableView;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *doneButton;
@property (strong, nonatomic) IBOutlet NSLayoutConstraint *navBarTopConstraint;
@property (strong, nonatomic) NSMutableArray *bookmarks;

@end

@implementation KMBookmarksViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self loadBookmarks];
    self.tableView.tableFooterView = [[UIView alloc] initWithFrame:CGRectZero];
}

- (void)viewWillAppear:(BOOL)animated{
    [super viewWillAppear:animated];
    
    self.navBarTopConstraint.constant = [AppDelegate statusBarHeight];
    [self checkIfEmpty];
}

- (void)didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation {
    self.navBarTopConstraint.constant = [AppDelegate statusBarHeight];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (IBAction)done:(id)sender {
    [self dismissViewControllerAnimated:YES completion:nil];
}

- (IBAction)addBookmark:(id)sender {
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Add Bookmark"
                                                    message:@""
                                                   delegate:self
                                          cancelButtonTitle:@"Cancel"
                                          otherButtonTitles:@"Add", nil];
    alert.alertViewStyle = UIAlertViewStyleLoginAndPasswordInput;
    
    UITextField *textField1 = [alert textFieldAtIndex:0];
    textField1.placeholder = @"Title";
    textField1.text = [self.webBrowser.webView stringByEvaluatingJavaScriptFromString:@"document.title"];
    textField1.autocapitalizationType = UITextAutocapitalizationTypeSentences;
    textField1.font = [UIFont systemFontOfSize:17];
    textField1.keyboardType = UIKeyboardTypeDefault;
    textField1.clearButtonMode = UITextFieldViewModeWhileEditing;
    
    UITextField *textField2 = [alert textFieldAtIndex:1];
    textField2.placeholder = @"Url";
    textField2.text = [self.webBrowser.webView.request.mainDocumentURL absoluteString];
    textField2.autocapitalizationType = UITextAutocapitalizationTypeNone;
    textField2.font = [UIFont systemFontOfSize:17];
    textField2.keyboardType = UIKeyboardTypeURL;
    textField2.clearButtonMode = UITextFieldViewModeWhileEditing;
    textField2.secureTextEntry = NO;
    
    [alert show];
}

- (void)checkIfEmpty {
    if ([self.tableView numberOfSections] == 0) {
        UILabel *label = [[UILabel alloc] initWithFrame:self.tableView.frame];
        [label setTextAlignment:NSTextAlignmentCenter];
        [label setTextColor:[UIColor lightGrayColor]];
        [label setFont:[UIFont systemFontOfSize:14.0]];
        label.text = @"No bookmarks";
        [self.tableView setBackgroundView:label];
    }
}

- (void)loadBookmarks {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    self.bookmarks = [[userData objectForKey:kKMWebBrowserBookmarksKey] mutableCopy];
    if (self.bookmarks == nil)
        self.bookmarks = [[NSMutableArray alloc] initWithCapacity:0];
}

- (void)saveBookmarks {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    if ([self.bookmarks count])
        [userData setObject:self.bookmarks forKey:kKMWebBrowserBookmarksKey];
    else
        [userData setObject:nil forKey:kKMWebBrowserBookmarksKey];
    [userData synchronize];
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    // Return the number of sections.
    NSInteger sections = [self.bookmarks count];
    if (sections > 0)
        self.tableView.backgroundView = nil;
    
    return sections;
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
    NSDictionary *bookmark = [self.bookmarks objectAtIndex:indexPath.section];
    cell.textLabel.text = [bookmark objectForKey:kBookmarkTitleKey];
    cell.detailTextLabel.text = [bookmark objectForKey:kBookmarkUrlKey];
    [cell setAccessoryType:UITableViewCellAccessoryNone];
}

- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
    return YES;
}

- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        [self.bookmarks removeObjectAtIndex:indexPath.section];
        [self saveBookmarks];
        [self.tableView reloadData];
        [self checkIfEmpty];
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    [tableView cellForRowAtIndexPath:indexPath].selected = NO;
    [self performActionForIndexPath:indexPath];
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath {
    [self performActionForIndexPath:indexPath];
}

- (void)performActionForIndexPath:(NSIndexPath *)indexPath {
    NSDictionary *bookmark = [self.bookmarks objectAtIndex:indexPath.section];
    NSString *urlString = [bookmark objectForKey:kBookmarkUrlKey];
    NSURL *url = [NSURL URLWithString:urlString];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    [self.webBrowser.webView loadRequest:request];
    [self dismissViewControllerAnimated:YES completion:nil];
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (buttonIndex != alertView.cancelButtonIndex) {
        UITextField *textField1 = [alertView textFieldAtIndex:0];
        if (![textField1.text length]) {
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Invalid Bookmark Title"
                                                            message:@"Please enter a valid title"
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
            [alert show];
            return;
        }
        
        UITextField *textField2 = [alertView textFieldAtIndex:1];
        if (![textField2.text length]) {
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Invalid Bookmark Url"
                                                            message:@"Please enter a valid Url"
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
            [alert show];
            return;
        }
        
        NSString *title = textField1.text;
        NSString *urlString = textField2.text;
        NSURL *url = [NSURL URLWithString:urlString];
        if (!url.scheme) {
            url = [NSURL URLWithString:[NSString stringWithFormat:@"http://%@", urlString]];
            urlString = [url absoluteString];
        }
        
        NSDictionary *bookmark = [[NSDictionary alloc] initWithObjectsAndKeys:
                                  title, kBookmarkTitleKey,
                                  urlString, kBookmarkUrlKey, nil];
        [self.bookmarks addObject:bookmark];
        [self saveBookmarks];
        [self.tableView reloadData];
    }
}

@end
