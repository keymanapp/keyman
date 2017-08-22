//
//  KMActivityProvider.m
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 14/11/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMActivityProvider.h"

NSString *const kKeymanHtmlMailFormat = @"<html><head><style type=\"text/css\">pre {font-family:\"%@\";font-size:%@;}</style></head><body><pre>%@</pre>%@</body></html>";
NSString *const kKeymanDefaultMailTextForPad = @"<br><br>Sent from&nbsp<a href=\"http://keyman.com/ipad\">Keyman for iPad</a>";
NSString *const kKeymanDefaultMailTextForPhone = @"<br><br>Sent from&nbsp<a href=\"http://keyman.com/iphone\">Keyman for iPhone</a>";
NSString *const kKeymanDefaultFbText = @"Can't read this? Help at http://keyman.com/fonts";

@implementation KMActivityProvider
@synthesize text, font;

- (id)activityViewController:(UIActivityViewController *)activityViewController itemForActivityType:(NSString *)activityType {
    if (text == nil) {
        text = @"";
    }
    
    if (font == nil) {
        font = [UIFont systemFontOfSize:[UIFont systemFontSize]];
    }
    
    if ([activityType isEqualToString:UIActivityTypeMail]) {
        //return [[NSAttributedString alloc] initWithString:text attributes:@{NSFontAttributeName:font}];
        return [self htmlMailWithText:text font:font];
    }
    else if ([activityType isEqualToString:UIActivityTypeMessage]) {
        return text;
    }
    else if ([activityType isEqualToString:UIActivityTypePostToFacebook]) {
        NSString *fbText = [NSString stringWithFormat:@"%@\n\n%@", text, kKeymanDefaultFbText];
        return fbText;
    }
    else if ([activityType isEqualToString:UIActivityTypePostToTwitter]) {
        if([text length] > 140)
            return [text substringToIndex:140];
        else
            return text;
    }
    else if ([activityType isEqualToString:UIActivityTypeCopyToPasteboard]) {
        return text;
    }
    else if ([activityType isEqualToString:@"KeymanActivity"]) {
        return @"";
    }
    else
        return nil;
}

- (id)activityViewControllerPlaceholderItem:(UIActivityViewController *)activityViewController {
    return @"";
}

- (NSString *)htmlMailWithText:(NSString *)txt font:(UIFont *)fnt {
    NSMutableString *mtext = [[NSMutableString alloc] initWithString:txt];
    [mtext replaceOccurrencesOfString:@"<" withString:@"&lt;" options:0 range:NSMakeRange(0, [mtext length])];
    [mtext replaceOccurrencesOfString:@">" withString:@"&gt;" options:0 range:NSMakeRange(0, [mtext length])];
    
    NSString *fname = [fnt familyName];
    NSString *fsize = [NSString stringWithFormat:@"%dpx", (int)[font pointSize]];
    
    NSString *dtext = nil;
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad) {
        dtext = [NSString stringWithString:kKeymanDefaultMailTextForPad];
    }
    else {
        dtext = [NSString stringWithString:kKeymanDefaultMailTextForPhone];
    }
    
    // html mail format: family-name, font-size, text, default-text
    NSString *htmlMail = [NSString stringWithFormat:kKeymanHtmlMailFormat, fname, fsize, mtext, dtext];
    
    return htmlMail;
}

@end

@implementation KMActivity

- (NSString *)activityType {
    return @"KeymanActivity";
}

- (NSString *)activityTitle {
    return @"Keyman";
}

- (UIImage *)activityImage {
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        return nil; //[UIImage imageNamed:@"keyman_home.png"];
    }
    else {
        return nil; //[UIImage imageNamed:@"keyman_home.png"];
    }
}

- (BOOL)canPerformWithActivityItems:(NSArray *)activityItems {
    return YES;
}

- (void)prepareWithActivityItems:(NSArray *)activityItems {

}

- (UIViewController *)activityViewController {
    return nil;
}

- (void)performActivity {
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString:@"http://keyman.com"]];
}

@end
