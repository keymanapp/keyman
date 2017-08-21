//
//  KMKeyboardMenuView.m
//  KeymanEngine
//
//  Created by Serkan Kurt on 18/03/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMKeyboardMenuView.h"
#import "KMManager+Internal.h"

static CGFloat borderRadius = 5.0; // 4.0;
static CGFloat strokeWidth = 0.75;

@interface KMKeyboardMenuView ()
@property (assign) CGRect menuFrame;
@property (assign) CGRect keyFrame;
@property (assign) CGFloat rowHeight;
@property (assign) CGFloat fontSize;
@property (assign) CGFloat xLength;
@property (assign) CGFloat adjX;
@property (assign) CGFloat topBarHeight;
@property (nonatomic, strong) UIColor *borderColor;
@property (nonatomic, strong) UIColor *bgColor;
@property (nonatomic, strong) UIColor *bgColor2;
@property (nonatomic, strong) UITableView *tableView;
@property (nonatomic, strong) NSMutableArray *tableList;
@property (nonatomic, strong) NSString *closeButtonTitle;
@property (nonatomic, weak) KMInputViewController *inputViewController;
@end

@implementation KMKeyboardMenuView
@synthesize tableView = _tableView;
@synthesize menuFrame, keyFrame, rowHeight, fontSize, xLength, adjX, topBarHeight;
@synthesize bgColor, bgColor2, borderColor;
@synthesize tableList = _tableList;

- (id)initWithKeyFrame:(CGRect)frame inputViewController:(KMInputViewController *)inputViewController closeButtonTitle:(NSString *)closeButtonTitle {
    BOOL isSystemKeyboard = [KMManager isKeymanSystemKeyboard];
    BOOL isPortrait = YES;
    if (isSystemKeyboard)
        isPortrait = [KMInputViewController isPortrait];
    else {
        UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
        isPortrait = UIInterfaceOrientationIsPortrait(orientation);
    }
    
    _inputViewController = inputViewController;
    _closeButtonTitle = closeButtonTitle;
    topBarHeight = [[KMManager sharedInstance] isSystemKeyboardTopBarEnabled]?[KMInputViewController topBarHeight]:0;
    keyFrame = CGRectInset(frame, 0, 0); // CGRectInset(frame, 2, 2);
    rowHeight = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?30:60;
    fontSize = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?14:21;
    xLength = keyFrame.size.width*(isPortrait?1.1:0.6);
    
    CGFloat screenWidth;
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 8)
        screenWidth = screenRect.size.width;
    else {
        if (isPortrait)
            screenWidth = screenRect.size.width;
        else
            screenWidth = screenRect.size.height;
    }
    
    CGFloat maxWidth = [self getMaxWidth];
    if (maxWidth > screenWidth)
        maxWidth = screenWidth;
    CGFloat baseHeight = keyFrame.size.height;
    CGFloat containerWidth = maxWidth - strokeWidth*2;
    CGFloat containerHeight = self.tableList.count*rowHeight;
    CGFloat vHeight = [KMManager keyboardHeight] + ([[KMManager sharedInstance] isSystemKeyboardTopBarEnabled]?[KMInputViewController topBarHeight]:0);
    CGFloat bY = [KMManager keyboardHeight] - (keyFrame.origin.y + baseHeight);

    if ((containerHeight + baseHeight ) > (vHeight - bY)) {
        int maxRows = (vHeight - baseHeight - bY)/rowHeight;
        containerHeight = maxRows*rowHeight;
    }
    
    CGRect containerFrame = CGRectMake(strokeWidth, strokeWidth, containerWidth, containerHeight);
    
    CGFloat viewWidth = maxWidth;
    CGFloat viewHeight = baseHeight + containerHeight + strokeWidth;
    CGFloat viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width)/2.0;
    CGFloat viewPosY = (keyFrame.origin.y + topBarHeight) - (viewHeight - keyFrame.size.height);
    
    adjX = 0;
    if (viewPosX < 0) {
        if ((keyFrame.origin.x - borderRadius*1.0 /* 2 */) < 0) {
            adjX = keyFrame.origin.x - viewPosX;
            viewPosX = keyFrame.origin.x;
        }
        else {
            adjX = -viewPosX;
            viewPosX = 0;
        }
    }
    else if ((viewPosX + viewWidth) > screenWidth) {
        if (((keyFrame.origin.x + keyFrame.size.width) + borderRadius*1.0 /* 2 */) > screenWidth) {
            adjX = viewPosX - keyFrame.origin.x;
            viewPosX = keyFrame.origin.x + 2*adjX;
        }
        else {
            adjX = (screenWidth - viewWidth) - viewPosX;
            viewPosX = adjX + viewPosX;
        }
    }
    
    CGRect mainFrame = self.inputViewController.view.frame;
    if (CGRectEqualToRect(mainFrame, CGRectZero))
        mainFrame = [KMManager inputView].frame;
    self = [super initWithFrame:mainFrame];
    if (self) {
        super.backgroundColor = [UIColor colorWithWhite:0.0 alpha:0.25];
        self.tag = 1;
        self.userInteractionEnabled = YES;
        bgColor = [[UIColor alloc] initWithRed:255.0/255.0
                                         green:255.0/255.0
                                          blue:255.0/255.0
                                         alpha:1.0];
        bgColor2 = [[UIColor alloc] initWithRed:255.0/255.0
                                          green:255.0/255.0
                                           blue:255.0/255.0
                                          alpha:1.0];
        borderColor = [[UIColor alloc] initWithRed:134.0/255.0 green:137.0/255.0 blue:139.0/255.0 alpha:1.0];
        
        menuFrame = CGRectMake(viewPosX, viewPosY, viewWidth, viewHeight);
        UIView *menuView = [[UIView alloc] initWithFrame:menuFrame];
        [menuView setTag:2];
        [menuView setBackgroundColor:[UIColor clearColor]];
        
        _tableView = [[UITableView alloc] initWithFrame:containerFrame];
        [_tableView setDelegate:self];
        [_tableView setDataSource:self];
        [_tableView setSeparatorStyle:UITableViewCellSeparatorStyleNone];
        [_tableView setBackgroundColor:[UIColor clearColor]];
        [_tableView.layer setCornerRadius:(borderRadius*1.0 /* 2 */ - strokeWidth)];
        [menuView addSubview:_tableView];
        [self addSubview:menuView];

        UITapGestureRecognizer *tapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapAction:)];
        [tapGesture setDelegate:self];
        [tapGesture setNumberOfTapsRequired:1];
        [tapGesture setNumberOfTouchesRequired:1];
        [self addGestureRecognizer:tapGesture];
    }
    
    return self;
}

- (void)drawRect:(CGRect)rect {
    CGFloat keyWidth = keyFrame.size.width;
    CGFloat keyHeight = keyFrame.size.height;
    CGFloat viewWidth = menuFrame.size.width;
    CGFloat viewHeight = menuFrame.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    //CGContextSetStrokeColorWithColor(context, [UIColor colorWithRed:1.0 green:0.0 blue:0.0 alpha:0.5].CGColor);
    CGFloat keyLeft = menuFrame.origin.x + (viewWidth - keyWidth)/2.0 + strokeWidth - adjX;
    CGFloat keyRight = menuFrame.origin.x + (viewWidth - keyWidth)/2.0 + keyWidth - strokeWidth - adjX;
    keyLeft = ceilf(keyLeft*10)/10;
    keyRight = floorf(keyRight*10)/10;
    
    CGFloat midY = menuFrame.origin.y + viewHeight - keyHeight;
    CGFloat viewLeft = menuFrame.origin.x + strokeWidth;
    CGFloat viewRight = menuFrame.origin.x + viewWidth - strokeWidth;
    CGFloat viewTop = menuFrame.origin.y + strokeWidth;
    CGFloat viewBottom = menuFrame.origin.y + viewHeight - strokeWidth*1.25;
    CGFloat viewMid = menuFrame.origin.x + viewWidth/2.0;
    CGFloat r = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*1.0 /* 2 */ - strokeWidth;
    CGFloat r3 = borderRadius*1.0 /* 2 */ - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY, r2);
    CGContextAddArcToPoint(context, viewLeft, midY, keyLeft, midY, r2);
    CGContextAddArcToPoint(context, keyLeft, midY, keyLeft, viewBottom, r3);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY, r);
    CGContextAddArcToPoint(context, keyRight, midY, viewRight, midY, r3);
    CGContextAddArcToPoint(context, viewRight, midY, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r2);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY, r2);
    CGContextAddArcToPoint(context, viewLeft, midY, keyLeft, midY, r2);
    CGContextAddArcToPoint(context, keyLeft, midY, keyLeft, viewBottom, r3);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY, r);
    CGContextAddArcToPoint(context, keyRight, midY, viewRight, midY, r3);
    CGContextAddArcToPoint(context, viewRight, midY, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r2);
    CGContextClosePath(context);
    CGContextClip(context);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor2.CGColor, nil];
    CGFloat gradientLocations[] = {0, 1};
    CGGradientRef gradient = CGGradientCreateWithColors(colorSpace, (__bridge CFArrayRef) gradientColors, gradientLocations);
    
    CGPoint startPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMinY(rect));
    CGPoint endPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMaxY(rect));
    
    CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
    CGGradientRelease(gradient);
    CGColorSpaceRelease(colorSpace);
    UIGraphicsPopContext();
}

- (BOOL)gestureRecognizerShouldBegin:(UIGestureRecognizer *)gestureRecognizer {
    CGPoint point = [gestureRecognizer locationInView:gestureRecognizer.view];
    UIView *view = [gestureRecognizer.view hitTest:point withEvent:nil];
    if (view.tag > 0)
        return YES;
    
    return NO;
}

- (void)tapAction:(UITapGestureRecognizer *)sender {
    [[KMManager sharedInstance] dismissKeyboardMenu];
}

- (NSMutableArray *)tableList {
    if (_tableList == nil) {
        NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
        NSMutableArray *keyboardList = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
        
        NSString *titleCloseButton;
        if (self.closeButtonTitle != nil)
            titleCloseButton = self.closeButtonTitle;
        else {
            NSString *appName = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleDisplayName"];
            titleCloseButton = [NSString stringWithFormat:@"Close %@", appName];
        }
        
        if (keyboardList == nil) {
            NSString *kbVersion = [[KMManager sharedInstance] latestKeyboardFileVersionWithID:kKeymanDefaultKeyboardID];
            _tableList = [[NSMutableArray alloc] initWithObjects:[NSDictionary dictionaryWithObjectsAndKeys:
                                                     kKeymanDefaultKeyboardID, kKeymanKeyboardIdKey,
                                                     kKeymanDefaultLanguageID, kKeymanLanguageIdKey,
                                                     kKeymanDefaultKeyboardName, kKeymanKeyboardNameKey,
                                                     kKeymanDefaultLanguageName, kKeymanLanguageNameKey,
                                                     kbVersion, kKeymanKeyboardVersionKey,
                                                     kKeymanDefaultKeyboardRTL, kKeymanKeyboardRTLKey,
                                                     kKeymanDefaultKeyboardFont, kKeymanFontKey,
                                                     nil], nil];
            [_tableList addObject:titleCloseButton];
        }
        else {
            _tableList = [[NSMutableArray alloc] initWithArray:keyboardList];
            /*
            _tableList = [[NSMutableArray alloc] initWithCapacity:keyboardList.count];
            int startIndex = 0;
            for (int i = 0; i < keyboardList.count; i++) {
                NSString *languageID = [[keyboardList objectAtIndex:i] objectForKey:kKeymanLanguageIdKey];
                NSString *keyboardID = [[keyboardList objectAtIndex:i] objectForKey:kKeymanKeyboardIdKey];
                if ([[KMManager sharedInstance].languageID isEqualToString:languageID] && [[KMManager sharedInstance].keyboardID isEqualToString:keyboardID]) {
                    startIndex = i;
                    break;
                }
            }
            
            for (int i = startIndex; i < keyboardList.count; i++) {
                [_tableList addObject:[keyboardList objectAtIndex:i]];
            }
            
            for (int i = 0; i < startIndex; i++) {
                [_tableList addObject:[keyboardList objectAtIndex:i]];
            }
            */
            
            [_tableList addObject:titleCloseButton];
        }
    }
    
    return _tableList;
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [self.tableList count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:cellIdentifier];
        UIView *selectionColor = [[UIView alloc] init];
        [selectionColor setBackgroundColor:[UIColor colorWithRed:74.0/255.0 green:186.0/255.0 blue:208.0/255.0 alpha:1.0]];
        [cell setSelectedBackgroundView:selectionColor];
    }
    
    return cell;
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath {
    //
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    return rowHeight;
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    [cell.textLabel setTextColor:[UIColor darkGrayColor]];
    [cell.textLabel setTextAlignment:NSTextAlignmentLeft];
    [cell.textLabel setFont:[UIFont systemFontOfSize:fontSize]];
    [cell setSelectionStyle:UITableViewCellSelectionStyleDefault];
    [cell setBackgroundColor:[UIColor clearColor]];
    if (indexPath.row == (self.tableList.count - 1)) {
        cell.textLabel.text = [self.tableList objectAtIndex:indexPath.row];
        cell.tag = indexPath.row;
        [cell.textLabel setTextColor:[UIColor grayColor]];
        [cell.textLabel setTextAlignment:NSTextAlignmentRight];
        [cell setSelected:NO];
        [cell setAccessoryType:UITableViewCellAccessoryDisclosureIndicator];
    }
    else {
        NSString *languageID = [[self.tableList objectAtIndex:indexPath.row] objectForKey:kKeymanLanguageIdKey];
        NSString *keyboardID = [[self.tableList objectAtIndex:indexPath.row] objectForKey:kKeymanKeyboardIdKey];
        cell.textLabel.text = [[self.tableList objectAtIndex:indexPath.row] objectForKey:kKeymanKeyboardNameKey];
        cell.tag = indexPath.row;
        if ([[KMManager sharedInstance].languageID isEqualToString:languageID] && [[KMManager sharedInstance].keyboardID isEqualToString:keyboardID]) {
            [cell setSelectionStyle:UITableViewCellSelectionStyleNone];
            [cell setSelected:YES];
            [cell setAccessoryType:UITableViewCellAccessoryCheckmark];
        }
        else {
            [cell setSelected:NO];
            [cell setAccessoryType:UITableViewCellAccessoryNone];
        }
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    [self switchKeyboard:indexPath.row];
    [[KMManager sharedInstance] dismissKeyboardMenu];
}

- (CGFloat)getMaxWidth {
    CGFloat w = 0;
    for (id obj in self.tableList) {
        NSString *text = nil;
        if ([obj isKindOfClass:[NSDictionary class]]) {
            NSDictionary *kb = (NSDictionary *)obj;
            text = [kb objectForKey:kKeymanKeyboardNameKey];
        }
        else {
            text = (NSString *)obj;
        }
        
        CGFloat tw = [self getTextWidth:text];
        if (tw > w)
            w = tw;
    }
    
    return w + (([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?75:150);
}

- (CGFloat)getTextWidth:(NSString *)text {
    return [text sizeWithAttributes:@{NSFontAttributeName: [UIFont systemFontOfSize:fontSize]}].width;
}

- (void)switchKeyboard:(NSInteger)index {
    if (index == (self.tableList.count - 1)) {
        [self.inputViewController advanceToNextInputMode];
    }
    else {
        NSString *langID = [[self.tableList objectAtIndex:index] objectForKey:kKeymanLanguageIdKey];
        NSString *kbID = [[self.tableList objectAtIndex:index] objectForKey:kKeymanKeyboardIdKey];
        NSString *langName = [[self.tableList objectAtIndex:index] objectForKey:kKeymanLanguageNameKey];
        NSString *kbName = [[self.tableList objectAtIndex:index] objectForKey:kKeymanKeyboardNameKey];
        NSString *font = [[self.tableList objectAtIndex:index] objectForKey:kKeymanFontKey];
        NSString *oskFont = [[self.tableList objectAtIndex:index] objectForKey:kKeymanOskFontKey];
    
        if ([[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont])
            [self.tableView reloadData];
    }
}

@end
