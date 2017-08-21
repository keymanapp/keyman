//
//  KeyboardNameTableViewCell.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMManager+Internal.h"


@interface KeyboardNameTableViewCell : UITableViewCell <UIAlertViewDelegate> {
	NSIndexPath *indexPath_;
}

@property (nonatomic, strong) NSIndexPath *indexPath;

- (void)setKeyboardState:(eKMKeyboardState)kbState selected:(BOOL)isSelected defaultAccessoryType:(UITableViewCellAccessoryType)accessoryType;

@end
