//
//  KMDropDownListView.m
//  Keyman
//
//  Created by Serkan Kurt on 16/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMDropDownListView.h"
#import <Keyman-Swift.h>

@implementation KMDropDownListView

- (id)initWithListItems:(NSArray *)items itemSize:(CGSize)size position:(CGPoint)pos {
    CGRect frame = CGRectMake(pos.x, pos.y, size.width, size.height);
    if (items.count)
        frame.size.height = items.count*size.height;

    self = [super initWithFrame:frame];
    if (self) {
        [self setBackgroundColor:[UIColor clearColor]];
        DropDownView *dropDownView = [[DropDownView alloc] initWithFrame:CGRectZero];
        CGFloat arrowHeight = dropDownView.arrowHeight;
        dropDownView.frame = CGRectMake(0, -arrowHeight, frame.size.width, frame.size.height+arrowHeight);
        [dropDownView setArrowPosX:frame.size.width-29];
        [self addSubview:dropDownView];
        
        NSUInteger count = items.count;
        CGFloat w = self.frame.size.width;
        CGFloat h = size.height;
        CGFloat x = 0;
        CGFloat y = 0;
        int index = 0;
        for (UIBarButtonItem *item in items) {
            UIButton *button = [UIButton buttonWithType:UIButtonTypeCustom];
            [button setTitleColor:[UIColor lightGrayColor] forState:UIControlStateNormal];
            [button setTitleColor:[UIColor darkGrayColor] forState:UIControlStateHighlighted];
            [button setFrame:CGRectMake(x, y, w, h)];
            [button setTitle:item.title forState:UIControlStateNormal];
            [self addSubview:button];
            NSArray *subViews = [[item customView] subviews];
            if ([subViews count]) {
                UIButton *btn = [[[item customView] subviews] objectAtIndex:0];
                UIImage *icon = [btn imageForState:UIControlStateNormal];
                UIImage *iconSelected = [btn imageForState:UIControlStateHighlighted];
                CGSize s = icon.size;
                CGFloat tLeft = (h - s.width);
                [button setImage:icon forState:UIControlStateNormal];
                [button setImage:iconSelected forState:UIControlStateHighlighted];
                [button setContentHorizontalAlignment:UIControlContentHorizontalAlignmentLeft];
                [button setContentEdgeInsets:UIEdgeInsetsMake(0, 8, 0, 0)];
                [button setTitleEdgeInsets:UIEdgeInsetsMake(0, tLeft, 0, 0)];
                for (id target in btn.allTargets) {
                    NSArray *actions = [btn actionsForTarget:target
                                                forControlEvent:UIControlEventTouchUpInside];
                    for (NSString *action in actions) {
                        [button addTarget:target
                                   action:NSSelectorFromString(action)
                         forControlEvents:UIControlEventTouchUpInside];
                    }
                }
            }
            
            if (index < (count - 1)) {
                UIView *seperator = [[UIView alloc] initWithFrame:CGRectMake(x+1, y+h, w-2, 1)];
                [seperator setBackgroundColor:[UIColor lightGrayColor]];
                [self addSubview:seperator];
            }
            
            y += h+1;
            index++;
        }
    }
    
    return self;
}
    
@end
