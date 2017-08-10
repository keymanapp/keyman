//
//  KMConfigColumn3CellView.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 26/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface KMConfigColumn3CellView : NSTableCellView
@property (nonatomic, weak) IBOutlet NSButton *infoButton;
@property (nonatomic, weak) IBOutlet NSButton *helpButton;
@property (nonatomic, weak) IBOutlet NSButton *removeButton;
@end
