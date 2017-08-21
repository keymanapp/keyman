//
//  KeyboardNameTableViewCell.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-06-19.
//  Updated by Serkan Kurt on 30/08/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardNameTableViewCell.h"

static const NSInteger kKeyboardCellIsSelectedViewTag = 4576; // arbitrary number
//static const NSInteger kKeymanMinFileSizeFor3GWarning = 2000000; // in bytes

@implementation KeyboardNameTableViewCell

@synthesize indexPath = indexPath_;


- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier {
	if (self = [super initWithStyle:style reuseIdentifier:reuseIdentifier]) {
		self.indentationLevel = 0;
	}
	return self;
}

- (void)setKeyboardState:(eKMKeyboardState)kbState selected:(BOOL)isSelected defaultAccessoryType:(UITableViewCellAccessoryType)accessoryType {
	switch (kbState) {
		case kKMKeyboardStateNeedsDownload:
		{
            self.accessoryView = nil;
			self.accessoryType = accessoryType;
			break;
		}
		case kKMKeyboardStateNeedsUpdate:
		{
            self.accessoryView = nil;
			self.accessoryType = accessoryType;
			break;
		}
		case kKMKeyboardStateUpToDate:
            self.accessoryView = nil;
			self.accessoryType = accessoryType;
			break;
		case kKMKeyboardStateDownloading:
		{
			//UIActivityIndicatorView *activityIndicator = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleGray];
			//self.accessoryView = activityIndicator;
			//self.accessoryType = UITableViewCellAccessoryNone;
			//[activityIndicator startAnimating];
            self.accessoryType = accessoryType;
			break;
		}
		case kKMKeyboardStateNone:
		default:
			self.accessoryView = nil;
			self.accessoryType = accessoryType;
			break;
	}
	
	UIView *isSelectedView = [self.contentView viewWithTag:kKeyboardCellIsSelectedViewTag];
	if (isSelected) {
		if (!isSelectedView) {
			isSelectedView = [[UIView alloc] initWithFrame:CGRectMake(0.0f, 0.0f, self.bounds.size.width, self.bounds.size.height)];
			isSelectedView.tag = kKeyboardCellIsSelectedViewTag;
			isSelectedView.backgroundColor = [UIColor greenColor];
			isSelectedView.autoresizingMask = UIViewAutoresizingFlexibleHeight|UIViewAutoresizingFlexibleWidth;
            [self.contentView addSubview:isSelectedView];
		}
	} else {
		[isSelectedView removeFromSuperview];
	}
}

@end
