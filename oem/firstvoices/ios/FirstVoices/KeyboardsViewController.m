//
//  KeyboardsViewController.m
//  FirstVoices
//
//  Created by Serkan Kurt on 17/11/2015.
//  Copyright © 2015 FirstVoices. All rights reserved.
//

#import "KeyboardsViewController.h"
#import "Keyman.h"
#import "FVShared.h"
#import "Checkbox.h"

NSString *const helpLink = @"http://help.keyman.com/keyboard/";
NSString *const kKeyboardsFileLastModDateKey = @"KeyboardsFileLastModDate";
NSInteger const tHelpButtonTag = 1000;
NSInteger const tCheckBoxTag = 2000;

@interface KeyboardsViewController ()
@property (nonatomic, weak) IBOutlet UITableView *tableView;
@property (nonatomic, strong) NSMutableArray *keyboardList;
@end

@implementation KeyboardsViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    [self.navigationController setNavigationBarHidden:NO];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return [self.keyboardList count]/2;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    NSInteger index = (section*2)+1;
    NSArray *kbArray = [self.keyboardList objectAtIndex:index];
    return [kbArray count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    static NSString *cellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];
    if (!cell) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:cellIdentifier];
        UIView *selectionColor = [[UIView alloc] init];
        [selectionColor setBackgroundColor:[UIColor colorWithRed:204.0/255.0 green:136.0/255.0 blue:34.0/255.0 alpha:1.0]];
        [cell setSelectedBackgroundView:selectionColor];
        [cell.textLabel setFont:[UIFont systemFontOfSize:14.0]];
    }
    
    if (cell.accessoryView == nil) {
        CGFloat cellH = CGRectGetHeight(cell.frame);
        CGFloat margin = 0.0;
        UIButton *helpButton = [UIButton buttonWithType:UIButtonTypeCustom];
        [helpButton setTag:tHelpButtonTag];
        [helpButton addTarget:self action:@selector(helpButtonTapped:forEvent:) forControlEvents:UIControlEventTouchUpInside];
        [helpButton setImage:[UIImage imageNamed:@"739-question"] forState:UIControlStateNormal];
        [helpButton setImage:[UIImage imageNamed:@"739-question-selected"] forState:UIControlStateHighlighted];
        CGFloat w1 = cellH;
        CGFloat h1 = cellH;
        [helpButton setFrame:CGRectMake(margin, 0, w1, h1)];
        
        CGFloat w2 = cellH-10;
        CGFloat h2 = cellH;
        Checkbox *checkBox = [[Checkbox alloc] initWithFrame:CGRectMake(margin+w1, 0, w2, h2)];
        [checkBox setTag:tCheckBoxTag];
        [checkBox addTarget:self action:@selector(checkBoxTapped:forEvent:) forControlEvents:UIControlEventValueChanged];
        checkBox.opaque = NO;
        checkBox.tintColor = [UIColor lightGrayColor];
        
        cell.accessoryView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, margin+w1+w2+margin, cellH)];
        [cell.accessoryView addSubview:helpButton];
        [cell.accessoryView addSubview:checkBox];
    }
    
    return cell;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section {
    return [self.keyboardList objectAtIndex:section*2];
}

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath {
    [cell setSelectionStyle:UITableViewCellSelectionStyleNone];
    [cell setAccessoryType:UITableViewCellAccessoryNone];
    
    NSInteger index = (indexPath.section*2)+1;
    NSArray *kbArray = [self.keyboardList objectAtIndex:index];
    NSDictionary *kbDict = [kbArray objectAtIndex:indexPath.row];
    cell.textLabel.text = [kbDict objectForKey:kFVKeyboardNameKey];
    
    NSString *checkState = [kbDict objectForKey:kFVKeyboardCheckStateKey];
    Checkbox *checkbox = (Checkbox *)[cell.accessoryView viewWithTag:tCheckBoxTag];
    [checkbox setChecked:[checkState isEqualToString:@"YES"]];
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    //
}

- (NSArray *)keyboardList {
    if (_keyboardList == nil) {
        NSUserDefaults *sharedData = [FVShared userDefaults];
        _keyboardList = [[sharedData arrayForKey:kFVKeyboardList] mutableCopy];
        NSArray *oldKeyboardList = nil;
        if (_keyboardList == nil || [self shouldLoadFromKeyboardsFile]) {
            @try {
                if (_keyboardList != nil) {
                    oldKeyboardList = [_keyboardList copy];
                    _keyboardList = nil;
                }
                NSString *keyboardsFile = [[NSBundle mainBundle] pathForResource:@"keyboards" ofType:@"csv"];
                NSString *fileContents = [[NSString stringWithContentsOfFile:keyboardsFile encoding:NSUTF8StringEncoding error:NULL] stringByReplacingOccurrencesOfString:@"\r" withString:@""];
                NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
                for (int i = 1; i < lines.count; i++) {
                    NSString *line = [lines objectAtIndex:i];
                    if (!line.length)
                        continue;
                    
                    NSArray *values = [line componentsSeparatedByString:@","];
                    if (values.count) {
                        if (_keyboardList == nil)
                            _keyboardList = [NSMutableArray arrayWithCapacity:0];
                        
                        NSString *kbName = values[0];
                        NSString *region = values[3];
                        NSString *langCode = values[9];
                        NSMutableString *kbFilename = [[values[7] stringByReplacingOccurrencesOfString:@".kmn" withString:@"-9.0.js"] mutableCopy];
                        
                        if (![_keyboardList containsObject:region]) {
                            [_keyboardList addObject:region];
                            NSMutableDictionary *kbDict = [[NSMutableDictionary alloc] initWithObjectsAndKeys:kbName, kFVKeyboardNameKey,
                                                           langCode, kFVKeyboardLanguageCodeKey,
                                                           kbFilename, kFVKeyboardFilenameKey,
                                                           @"NO", kFVKeyboardCheckStateKey, nil];
                            [_keyboardList addObject:[NSMutableArray arrayWithObject:kbDict]];
                        }
                        else {
                            NSUInteger index = [_keyboardList indexOfObject:region];
                            index++;
                            NSMutableArray *kbArray = [_keyboardList objectAtIndex:index];
                            NSMutableDictionary *kbDict = [[NSMutableDictionary alloc] initWithObjectsAndKeys:kbName, kFVKeyboardNameKey,
                                                           langCode, kFVKeyboardLanguageCodeKey,
                                                           kbFilename, kFVKeyboardFilenameKey,
                                                           @"NO", kFVKeyboardCheckStateKey, nil];
                            [kbArray addObject:kbDict];
                        }
                    }
                }
                
                NSUserDefaults *sharedData = [FVShared userDefaults];
                [sharedData setObject:_keyboardList forKey:kFVKeyboardList];
                [sharedData setObject:[NSArray arrayWithObject:[self keyboardsFileModDate]] forKey:kKeyboardsFileLastModDateKey];
                [sharedData synchronize];
            }
            @catch (NSException *e) {
                NSLog(@"Error = %@", e.description);
                _keyboardList = nil;
            }
        }
        
        if (oldKeyboardList != nil)
            [self restoreActiveKeyboardsFromArray:oldKeyboardList];
    }
    
    return _keyboardList;
}

- (BOOL)shouldLoadFromKeyboardsFile {
    NSUserDefaults *sharedData = [FVShared userDefaults];
    NSDate *lastModDate = [[sharedData objectForKey:kKeyboardsFileLastModDateKey] objectAtIndex:0];
    if (lastModDate == nil)
        return YES;
    
    NSDate *fileModDate = [self keyboardsFileModDate];
    if (fileModDate == nil)
        return YES;
    
    if ([fileModDate timeIntervalSinceDate:lastModDate] > 0)
        return YES;
    
    return NO;
}

- (NSDate *)keyboardsFileModDate {
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"keyboards" ofType:@"csv"];
    NSDictionary *attrs = [[NSFileManager defaultManager] attributesOfItemAtPath:filePath error:nil];
    return [attrs objectForKey:NSFileModificationDate];
}

- (void)helpButtonTapped:(id)sender forEvent:(UIEvent*)event {
    NSSet *touches = [event allTouches];
    UITouch *touch = [touches anyObject];
    CGPoint currentTouchPosition = [touch locationInView:self.tableView];
    
    NSIndexPath *indexPath = [self.tableView indexPathForRowAtPoint:currentTouchPosition];
    if (indexPath != nil) {
        NSInteger index = (indexPath.section*2)+1;
        NSArray *kbArray = [self.keyboardList objectAtIndex:index];
        NSDictionary *kbDict = [kbArray objectAtIndex:indexPath.row];
        NSString *kbFilename = [kbDict objectForKey:kFVKeyboardFilenameKey];
        NSString *kbID = [kbFilename substringToIndex:[kbFilename rangeOfString:@"-"].location];
        NSURL *helpUrl = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@", helpLink, kbID]];
        [[UIApplication sharedApplication] openURL:helpUrl];
    }
}

- (void)checkBoxTapped:(id)sender forEvent:(UIEvent*)event {
    NSSet *touches = [event allTouches];
    UITouch *touch = [touches anyObject];
    CGPoint currentTouchPosition = [touch locationInView:self.tableView];
    
    NSIndexPath *indexPath = [self.tableView indexPathForRowAtPoint:currentTouchPosition];
    if (indexPath != nil) {
        // Update the checkState with the new checked state
        NSInteger index = (indexPath.section*2)+1;
        NSMutableArray *kbArray = [[self.keyboardList objectAtIndex:index] mutableCopy];
        NSMutableDictionary *kbDict = [[kbArray objectAtIndex:indexPath.row] mutableCopy];
        
        [kbDict setObject:[(Checkbox *)sender isChecked]?@"YES":@"NO" forKey:kFVKeyboardCheckStateKey];
        [kbArray replaceObjectAtIndex:indexPath.row withObject:kbDict];
        [_keyboardList replaceObjectAtIndex:index withObject:kbArray];
        
        NSUserDefaults *sharedData = [FVShared userDefaults];
        [sharedData setObject:_keyboardList forKey:kFVKeyboardList];
        [sharedData synchronize];
        
        [self updateActiveKeyboardsList];
    }
}

- (void)updateActiveKeyboardsList {
    NSUserDefaults *sharedData = [FVShared userDefaults];
    [sharedData removeObjectForKey:kKeymanUserKeyboardsListKey];
    NSArray *kbList = self.keyboardList;
    for (int i = 0; i < kbList.count; i+=2) {
        NSArray *kbArray = [kbList objectAtIndex:i+1];
        for (NSDictionary *kbDict in kbArray) {
            if ([[kbDict objectForKey:kFVKeyboardCheckStateKey] isEqualToString:@"YES"]) {
                NSString *kbFilename = [kbDict objectForKey:kFVKeyboardFilenameKey];
                NSString *kbID = [kbFilename substringToIndex:[kbFilename rangeOfString:@"-"].location];
                NSString *langCode = [kbDict objectForKey:kFVKeyboardLanguageCodeKey];
                NSString *langID = [[langCode substringToIndex:(langCode.length<3?langCode.length:3)] lowercaseString];
                NSString *kbName = [kbDict objectForKey:kFVKeyboardNameKey];
                NSString *langName = kbName;
                [[KMManager sharedInstance] addKeyboardWithID:kbID
                                                   languageID:langID
                                                 keyboardName:kbName
                                                 languageName:langName
                                                        isRTL:NO
                                                     isCustom:YES
                                                         font:nil
                                                      oskFont:nil];
            }
        }
    }
    
    if (![sharedData objectForKey:kKeymanUserKeyboardsListKey]) {
        [[KMManager sharedInstance] addKeyboardWithID:kKeymanDefaultKeyboardID
                                           languageID:kKeymanDefaultLanguageID
                                         keyboardName:kKeymanDefaultKeyboardName
                                         languageName:kKeymanDefaultLanguageName
                                                isRTL:[kKeymanDefaultKeyboardRTL isEqualToString:@"Y"]?YES:NO
                                             isCustom:NO
                                                 font:kKeymanDefaultKeyboardFont
                                              oskFont:nil];
    }
}

- (void)restoreActiveKeyboardsFromArray:(NSArray *)oldKeyboardList {
    NSMutableArray *oldActiveKeyboards = [NSMutableArray arrayWithCapacity:0];
    for (int i = 0; i < oldKeyboardList.count; i+=2) {
        NSArray *kbArray = [oldKeyboardList objectAtIndex:i+1];
        for (NSDictionary *kbDict in kbArray) {
            if ([[kbDict objectForKey:kFVKeyboardCheckStateKey] isEqualToString:@"YES"]) {
                NSString *kbFilename = [kbDict objectForKey:kFVKeyboardFilenameKey];
                NSString *kbID = [kbFilename substringToIndex:[kbFilename rangeOfString:@"-"].location];
                [oldActiveKeyboards addObject:kbID];
            }
        }
    }
    
    for (int i = 0; i < self.keyboardList.count; i+=2) {
        NSArray *kbArray = [self.keyboardList objectAtIndex:i+1];
        int x = 0;
        for (NSDictionary *kbDict in kbArray) {
            NSString *kbFilename = [kbDict objectForKey:kFVKeyboardFilenameKey];
            NSString *kbID = [kbFilename substringToIndex:[kbFilename rangeOfString:@"-"].location];
            if ([oldActiveKeyboards containsObject:kbID]) {
                NSMutableArray *nArray = [kbArray mutableCopy];
                NSMutableDictionary *nDict = [kbDict mutableCopy];
                [nDict setObject:@"YES" forKey:kFVKeyboardCheckStateKey];
                [nArray replaceObjectAtIndex:x withObject:nDict];
                [_keyboardList replaceObjectAtIndex:i+1 withObject:nArray];
            }
            x++;
        }
    }
    
    [self updateActiveKeyboardsList];
}

@end
