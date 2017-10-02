//
//  MainViewController.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMTextView.h"
#import "KMTextField.h"

@interface MainViewController : UIViewController <KMTextViewDelegate> {
	KMTextView *textView1_;
	KMTextField *textView2_;
	UITextView *textView3_;
}

@property (nonatomic, strong) KMTextView *textView1;
@property (nonatomic, strong) KMTextField *textView2;
@property (nonatomic, strong) UITextView *textView3;

- (id)autoreleasedTextViewOfClass:(Class)theClass;
- (UIScrollView *)contentView;

@end
