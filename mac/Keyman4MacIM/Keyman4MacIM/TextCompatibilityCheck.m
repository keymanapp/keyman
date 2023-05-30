/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * TextCompatibilityCheck.m
 * KeyTest
 * 
 * Created by Shawn Schantz on 2023-05-05.
 * 
 * Check what APIs are available for getting location/selection and getting context
 */

#import "TextCompatibilityCheck.h"
#import <InputMethodKit/InputMethodKit.h>

@interface TextCompatibilityCheck()

@property (readonly) id client;
@property (readonly) NSString *clientApplicationId;
@property (readonly) BOOL hasWorkingSelectionApi;
@property (readonly) BOOL hasReadApi;
@property (readonly) BOOL hasInsertApi;

@end


@implementation TextCompatibilityCheck

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId  {
  self = [super init];
  if (self) {
    _client = client;
    _clientApplicationId = appId;
    _hasWorkingSelectionApi = [self checkSelectionApi: client applicationId:appId];
    _hasReadApi = [client respondsToSelector:@selector(attributedSubstringFromRange:)];
    _hasInsertApi = [client respondsToSelector:@selector(insertText:replacementRange:)];

  }
  return self;
}

-(NSString *)description
{
return [NSString stringWithFormat:@"hasSelectionAPI = %d, hasReadAPI - %d, hasInsertAPI = %d, canGetSelection = %d, canReadText = %d, canInsertText = %d, canReplaceText = %d, mustBackspaceUsingEvents = %d, clientAppId: %@, client: %@", self.hasWorkingSelectionApi, self.hasReadApi, self.hasInsertApi, [self canGetSelection], [self canReadText], [self canInsertText], [self canReplaceText], [self mustBackspaceUsingEvents], _clientApplicationId, _client];
}

/** returns true if the API selectedRange is determined to be broken because it either fails the test or is included in our hard-coded list of legacy apps */
-(BOOL) checkSelectionApi:(id) client applicationId:(NSString *)clientAppId  {
  BOOL workingSelectionApi = NO;

  // if the selector exists, then call the API and see if it returns a valid value
  
  if ([client respondsToSelector:@selector(selectedRange)]) {
    NSRange selectionRange = [self.client selectedRange];
    NSLog(@"TextCompatibilityCheck checkSelectionApi, location = %lu, length = %lu", selectionRange.location, selectionRange.length);

    NSRange notFoundRange = NSMakeRange(NSNotFound, NSNotFound);

    /*
    if (selectionRange.location == 0) {
      canGetSelection = NO;
      NSLog(@"checkCanGetSelection, selectionRange.location == 0 ");
    } else
     */
    if (NSEqualRanges(selectionRange, notFoundRange)) {
      workingSelectionApi = NO;
      NSLog(@"TextCompatibilityCheck checkSelectionApi, range is NSNotFound");
    } else {
      workingSelectionApi = YES;
    }
  }

  // TODO: integrate with existing legacy code
  // if the selection API appears to work, it may still be broken
  // getting the selection does not work for anything in the legacy app list
  if (workingSelectionApi) {
    BOOL isLegacy = ([clientAppId isEqual: @"com.github.atom"] ||
                     [clientAppId isEqual: @"com.collabora.libreoffice-free"] ||
                     [clientAppId isEqual: @"org.libreoffice.script"] ||
                     [clientAppId isEqual: @"com.axosoft.gitkraken"] ||
                     [clientAppId isEqual: @"org.sil.app.builder.scripture.ScriptureAppBuilder"] ||
                     [clientAppId isEqual: @"org.sil.app.builder.reading.ReadingAppBuilder"] ||
                     [clientAppId isEqual: @"org.sil.app.builder.dictionary.DictionaryAppBuilder"] ||
                     //[clientAppId isEqual: @"com.microsoft.Word"] || // 2020-11-24[mcd]: Appears to work well in Word 16.43, disable legacy by default
                     [clientAppId isEqual: @"org.openoffice.script"] ||
                     [clientAppId isEqual: @"com.adobe.illustrator"] ||
                     [clientAppId isEqual: @"com.adobe.InDesign"] ||
                     [clientAppId isEqual: @"com.adobe.Photoshop"] ||
                     [clientAppId isEqual: @"com.adobe.AfterEffects"] ||
                     //[clientAppId isEqual: @"com.microsoft.VSCode"] || // 2023-05-29[sgs]: Works with 1.78.2, disable legacy by default
                     [clientAppId isEqual: @"com.google.Chrome"] ||
                     [clientAppId hasPrefix: @"net.java"] ||
                     [clientAppId isEqual: @"com.Keyman.test.legacyInput"]
                     /*||[clientAppId isEqual: @"ro.sync.exml.Oxygen"] - Oxygen has worse problems */
                     );
    workingSelectionApi = !isLegacy;
  }
  return workingSelectionApi;
}

-(BOOL) canGetSelection {
  return self.hasWorkingSelectionApi;
}

-(BOOL) canReadText {
  // seems simple, but every application tested that returned the selection also successfully read the text using attributedSubstringFromRange
  BOOL canReadText = self.hasReadApi && [self canGetSelection];
  return canReadText;
}

-(BOOL) canInsertText {
  // all applications tested can insertText (though replacement may not work)
  return self.hasInsertApi;
}

-(BOOL) canReplaceText {
  // testing shows that every application that returns the selection, can also replace text
  return self.hasInsertApi && [self canGetSelection];
}

-(BOOL) mustBackspaceUsingEvents {
  // if we cannot replace text, then we always need to use events to backspace
  return ![self canReplaceText];
}

/*
-(void) experiment {
  NSRange notFoundRange = NSMakeRange(NSNotFound, NSNotFound);
  NSRange zeroRange = NSMakeRange(0, 0);
  NSRange selectionRange = [self.client selectedRange];

  if (NSEqualRanges(selectionRange, notFoundRange)) {
    NSLog(@"**compatibility** ERROR selectedRange = {NSNotFound, NSNotFound}");
  } else if (NSEqualRanges(selectionRange, notFoundRange)) {
      NSLog(@"**compatibility** ERROR selectedRange = {0,0}");
  } else {
    NSLog(@"**compatibility** selectedRange = %@", NSStringFromRange(selectionRange));
  }
  
  NSRange contextRange = NSMakeRange(1, 0);
  NSLog(@"**compatibility** get string of empty range, location 1, length 0");
  
  NSAttributedString *attributedText = [self.client attributedSubstringFromRange:contextRange];

  if (attributedText == nil) {
    NSLog(@"  **compatibility** attributedSubstringFromRange({1,0}) is nil");
  } else {
    NSString *context = attributedText.string;
    NSLog(@"  **compatibility** attributedSubstringFromRange = %@", context);
  }

  contextRange = NSMakeRange(1, 1);
  NSLog(@"**compatibility** get string at location 1, length 1");
  
  attributedText = [self.client attributedSubstringFromRange:contextRange];

  if (attributedText == nil) {
    NSLog(@"  **compatibility** attributedSubstringFromRange({1,1}) is nil");
  } else {
    NSString *context = attributedText.string;
    NSLog(@"  **compatibility** attributedSubstringFromRange = %@", context);
  }
}
*/
@end
