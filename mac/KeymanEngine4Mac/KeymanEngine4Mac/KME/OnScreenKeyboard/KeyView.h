//
//  KeyView.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "OSKKey.h"
#import "KMkeyViewProtocol.h"

@interface KeyView : NSView

@property (nonatomic, strong) OSKKey *key;
@property (nonatomic, strong) id <KMkeyView> target;
@property (nonatomic, strong) NSImage *bitmap;
@property (nonatomic, assign) BOOL keyPressed;

- (void)setLabelText:(NSString *)text;
- (void)setLabelFont:(NSString *)fontName;
- (void)setCaptionText:(NSString *)text;
- (void)setCaptionFont:(NSString *)fontName;

@end
