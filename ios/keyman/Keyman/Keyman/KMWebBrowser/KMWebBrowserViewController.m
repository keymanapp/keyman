//
//  KMWebBrowserViewController.m
//  Keyman
//
//  Created by Serkan Kurt on 9/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMWebBrowserViewController.h"
#import "AppDelegate.h"
#import "UIImage+Helpers.h"
#import "KMManager.h"
#import <Keyman-Swift.h>

@interface KMWebBrowserViewController ()

@property (strong, nonatomic) IBOutlet UINavigationBar *navBar;
@property (strong, nonatomic) IBOutlet UIToolbar *toolBar;
@property (strong, nonatomic) IBOutlet NSLayoutConstraint *navBarTopConstraint;
@property (strong, nonatomic) UITextField *addressField;
@property (strong, nonatomic) UIView *rightView;
@property (strong, nonatomic) UIButton *refreshButton;
@property (strong, nonatomic) UIButton *stopButton;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *backButton;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *forwardButton;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *bookmarksButton;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *globeButton;
@property (strong, nonatomic) IBOutlet UIBarButtonItem *closeButton;
@property (strong, nonatomic) NSString *nFontFamily;

@end

@implementation KMWebBrowserViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardPickerDismissed:) name:kKeymanKeyboardPickerDismissedNotification object:nil];
    
    self.webView.delegate = self;
    self.webView.scalesPageToFit = YES;
    
    // Setup NavigationBar
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        CGFloat size = screenRect.size.height > screenRect.size.width?screenRect.size.height:screenRect.size.width;
        if (size > 568.0) {
            // Navbar for iPhone 6 & 6 Plus
            UIImage *bgImg = nil;
            if (UIInterfaceOrientationIsPortrait(orientation))
                bgImg = [[UIImage imageNamed:@"kmwb-navbar-Portrait.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            else
                bgImg = [[UIImage imageNamed:@"kmwb-navbar-Landscape-568h.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            
            [self.navBar setBackgroundImage:bgImg forBarMetrics:UIBarMetricsDefault];
            //[self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Landscape-568h.png"] forBarMetrics:UIBarMetricsCompact];
        }
        else if (size == 568) {
            // Navbar for iPhone and iPod Touch with 4" Display
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Landscape-568h.png"] forBarMetrics:UIBarMetricsCompact];
        }
        else if (size < 568.0) {
            // Navbar for iPhone and iPod Touch with 3.5" Display
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Landscape.png"] forBarMetrics:UIBarMetricsCompact];
        }
    }
    else {
        // Navbar for iPad
        if (UIInterfaceOrientationIsPortrait(orientation))
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
        else
            [self.navBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Landscape.png"] forBarMetrics:UIBarMetricsDefault];
    }

    // Setup address field
    CGRect addressFrame = CGRectMake(10, 4.0, self.navBar.frame.size.width - 20, 28);
    self.addressField = [[UITextField alloc] initWithFrame:addressFrame];
    self.addressField.clearButtonMode = UITextFieldViewModeWhileEditing;
    self.addressField.rightViewMode = UITextFieldViewModeUnlessEditing;
    self.addressField.autoresizingMask = UIViewAutoresizingFlexibleWidth;
    self.addressField.borderStyle = UITextBorderStyleRoundedRect;
    self.addressField.autocapitalizationType = UITextAutocapitalizationTypeNone;
    self.addressField.autocorrectionType = UITextAutocorrectionTypeNo;
    self.addressField.font = [UIFont systemFontOfSize:17];
    self.addressField.placeholder = @"http://";
    self.addressField.text = @"";
    self.addressField.keyboardType = UIKeyboardTypeURL;
    [self.addressField addTarget:self
                          action:@selector(loadAddress:event:)
                forControlEvents:UIControlEventEditingDidEndOnExit];
    
    [self.navBar addSubview:self.addressField];
    
    self.rightView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 40, 28)];
    self.addressField.rightView = self.rightView;
    
    UIImage *refreshIcon = [[UIImage imageNamed:@"UIButtonBarRefresh.png"]
                                    scaleToSize:CGSizeMake(18, 22)];
    if ([UIImage instancesRespondToSelector:@selector(imageWithRenderingMode:)])
        refreshIcon = [refreshIcon imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal];
    self.refreshButton = [UIButton buttonWithType:UIButtonTypeSystem];
    [self.refreshButton setImage:refreshIcon forState:UIControlStateNormal];
    [self.refreshButton setFrame:self.rightView.frame];
    [self.refreshButton addTarget:self
                           action:@selector(refresh:)
                 forControlEvents:UIControlEventTouchUpInside];
    [self.refreshButton setHidden:YES];
    [self.rightView addSubview:self.refreshButton];
    
    UIImage *stopIcon = [[UIImage imageNamed:@"UIButtonBarStop.png"]
                                 scaleToSize:CGSizeMake(17, 17)];
    if ([UIImage instancesRespondToSelector:@selector(imageWithRenderingMode:)])
        stopIcon = [stopIcon imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal];
    
    self.stopButton = [UIButton buttonWithType:UIButtonTypeSystem];
    [self.stopButton setImage:stopIcon forState:UIControlStateNormal];
    [self.stopButton setFrame:self.rightView.frame];
    [self.stopButton addTarget:self
                        action:@selector(stop:)
              forControlEvents:UIControlEventTouchUpInside];
    [self.stopButton setHidden:YES];
    [self.rightView addSubview:self.stopButton];
    
    [self updateButtons];
    
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    NSString *lastUrlStr = [userData objectForKey:@"KMWebBrowserLastURL"];
    if (lastUrlStr == nil)
        lastUrlStr = @"http://www.google.com/";
    NSURL *url = [NSURL URLWithString:lastUrlStr];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    [self.webView loadRequest:request];
}

- (void)viewWillAppear:(BOOL)animated{
    [super viewWillAppear:animated];
    
    self.navBarTopConstraint.constant = [AppDelegate statusBarHeight];
}

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    UIInterfaceOrientation orientation = toInterfaceOrientation;
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        CGRect screenRect = [[UIScreen mainScreen] bounds];
        CGFloat size = screenRect.size.height > screenRect.size.width?screenRect.size.height:screenRect.size.width;
        if (size > 568.0) {
            // Navbar for iPhone 6 & 6 Plus
            UIImage *bgImg = nil;
            if (UIInterfaceOrientationIsPortrait(orientation))
                bgImg = [[UIImage imageNamed:@"kmwb-navbar-Portrait.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            else
                bgImg = [[UIImage imageNamed:@"kmwb-navbar-Landscape-568h.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            
            [self.navigationController.navigationBar setBackgroundImage:bgImg
                                                          forBarMetrics:UIBarMetricsDefault];
        }
    }
    else {
        if (UIInterfaceOrientationIsPortrait(orientation))
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
        else
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"kmwb-navbar-Landscape.png"] forBarMetrics:UIBarMetricsDefault];
    }
}

- (void)didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation {
    self.navBarTopConstraint.constant = [AppDelegate statusBarHeight];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)loadAddress:(id)sender event:(UIEvent *)event {
    NSString *urlString = self.addressField.text;
    NSURL *url = [NSURL URLWithString:urlString];
    if (!url.scheme)
        url = [NSURL URLWithString:[NSString stringWithFormat:@"http://%@", urlString]];
    
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    [self.webView loadRequest:request];
}

- (void)updateAddress:(NSURLRequest*)request {
    NSURL *url = [request mainDocumentURL];
    self.addressField.text = [url absoluteString];
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:[url absoluteString] forKey:@"KMWebBrowserLastURL"];
    [userData synchronize];
}

- (void)refresh:(id)sender {
    [self.webView reload];
}

- (void)stop:(id)sender {
    [self.webView stopLoading];
}

- (IBAction)back:(id)sender {
    [self.webView goBack];
}

- (IBAction)forward:(id)sender {
    [self.webView goForward];
}

- (IBAction)bookmarks:(id)sender {
    BookmarksViewController *bookmarksVC = [[BookmarksViewController alloc] init];
    bookmarksVC.webBrowser = self;
    [self presentViewController:bookmarksVC animated:YES completion:nil];
}

- (IBAction)globe:(id)sender {
    [[KMManager sharedInstance] showKeyboardPickerInViewController:self shouldAddKeyboard:NO];
}

- (IBAction)close:(id)sender {
    [self dismissViewControllerAnimated:YES completion:nil];
}

#pragma mark - UIWebViewDelegate methods

- (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType {
    [self updateAddress:request];
    return YES;
}

- (void)webViewDidStartLoad:(UIWebView *)webView {
    [UIApplication sharedApplication].networkActivityIndicatorVisible = YES;
    [self updateButtons];
}

- (void)webViewDidFinishLoad:(UIWebView *)webView {
    [UIApplication sharedApplication].networkActivityIndicatorVisible = NO;
    [self appendCSSFontFamily];
    [self updateButtons];
}

- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error {
    [UIApplication sharedApplication].networkActivityIndicatorVisible = NO;
    [self updateButtons];
    
    if (error.code == -1003 || error.code == -1009) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Cannot Open Page"
                                                    message:error.localizedDescription
                                                   delegate:self
                                          cancelButtonTitle:@"OK"
                                          otherButtonTitles:nil];
        [alert show];
    }
    
    /*
    NSString *htmlFormat =
    @"<html>"
    @"<head>"
    @"<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">"
    @"<meta name=\"format-detection\" content=\"telephone=no\">"
    @"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no\">"
    @"<style>"
    @"body {"
    @"    -webkit-user-select: none;"
    @"    -webkit-text-size-adjust: none;"
    @"    font-family: -apple-system-font;"
    @"    font-size: 17px;"
    @"    color: rgb(125, 127, 127);"
    @"    height: 100%;"
    @"    margin: 0;"
    @"    padding: 0;"
    @"}"
    @"p{"
    @"    text-align:center;"
    @"    word-wrap: break-word;"
    @"    margin: 123px 50px 0 50px;"
    @"}"
    @"</style>"
    @"<title>%@</title>"
    @"</head>"
    @"<body>"
    @"<p>%@</p>"
    @"</body>"
    @"</html>";
    if (error.code == -1003 || error.code == -1009) {
        NSString *html = [NSString stringWithFormat:htmlFormat,
                          @"Cannot Open Page", error.localizedDescription];
        [self.webView loadHTMLString:html baseURL:self.webView.request.URL];
    }
    */
}

- (void)updateButtons {
    self.refreshButton.hidden = self.webView.loading;
    self.stopButton.hidden = !self.webView.loading;
    self.backButton.enabled = self.webView.canGoBack;
    self.forwardButton.enabled = self.webView.canGoForward;
}

- (void)appendCSSFontFamily {
    NSString *jsStr = [NSString stringWithFormat:
    @"var style = document.createElement('style');"
    "style.type = 'text/css';"
    "style.innerHTML = '*{font-family:\"%@\" !important;}';"
    "document.getElementsByTagName('head')[0].appendChild(style);", self.fontFamily];
    [self.webView stringByEvaluatingJavaScriptFromString:jsStr];
}

- (void)setFontFamily:(NSString *)fontFamily {
    _fontFamily = fontFamily;
    _nFontFamily = [NSString stringWithString:fontFamily];
}

- (void)keyboardChanged:(NSNotification *)notification {
    NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
    NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
    NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
    NSString *fontName = [[KMManager sharedInstance] fontNameForKeyboardWithID:kbID languageID:langID];
    if (fontName != nil)
        _nFontFamily = [NSString stringWithString:fontName];
    else
        _nFontFamily = [NSString stringWithString:[UIFont systemFontOfSize:[UIFont systemFontSize]].fontName];
}

- (void)keyboardPickerDismissed:(NSNotification *)notification {
    if (![_nFontFamily isEqualToString:_fontFamily]) {
        self.fontFamily = [NSString stringWithString:_nFontFamily];
        [self.webView reload];
    }
}

@end
