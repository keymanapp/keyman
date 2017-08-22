//
//  UIImage+Helpers.h
//  Keyman
//
//  Created by Serkan Kurt on 11/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface UIImage (Helpers)

- (UIImage *)scaleToSize:(CGSize)newSize;
- (UIImage *)tintedImageUsingColor:(UIColor *)tintColor;

@end
