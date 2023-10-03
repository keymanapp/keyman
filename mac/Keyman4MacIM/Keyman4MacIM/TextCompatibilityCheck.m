/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * TextCompatibilityCheck.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-05.
 * 
 * Check whether the current text input client is compatibility with APIs.
 * Compliance is determined by attempting the current selection (location and length),
 * but this is not a surefire indicator. For some apps,  a valid but incorrect selection of
 * {0, 0} is returned regardless of the real selection.
 *
 * When {0, 0} is returned, then we can check to see if a subsequent insert results in a
 * change in the location. It always should, because it cannot replace at location 0.
 * If the location does change, then the selection API is compliant.
 *
 * Even worse for some apps, the selection API works, but attempts
 * to replace during an insert do not work. There is no way to detect this behavior,
 * so these apps must be hard-coded as non-compliant. A couple of known apps
 * that behave this way are Brackets (Adobe OS project) and MacVIM
 */

#import "TextCompatibilityCheck.h"
#import <InputMethodKit/InputMethodKit.h>

NSString *const kKMLegacyApps = @"KMLegacyApps";

@interface TextCompatibilityCheck()

@property (readonly) id client;
@property BOOL apiComplianceUncertain;
@property BOOL hasCompliantSelectionApi;
@property (readonly) BOOL hasReadApi;
@property (readonly) BOOL hasInsertApi;

@end

@implementation TextCompatibilityCheck

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId {
  self = [super init];
  if (self) {
    _client = client;
    _clientApplicationId = appId;
    _apiComplianceUncertain = YES;
    
    // first check in the noncompliant app lists
    // TODO: uncomment after testing
    //BOOL isUncompliantApp = ![self containedInNoncompliantAppLists:clientAppId];
    BOOL isUncompliantApp = NO;
    
    if (isUncompliantApp) {
      _apiComplianceUncertain = NO;
      self.hasCompliantSelectionApi = NO;
    } else {
      [self testApiCompliance:client];
    }
    
    _hasReadApi = [client respondsToSelector:@selector(attributedSubstringFromRange:)];
    _hasInsertApi = [client respondsToSelector:@selector(insertText:replacementRange:)];
  }
  return self;
}

-(NSString *)description
{
return [NSString stringWithFormat:@"apiComplianceUncertain: %d, hasWorkingSelectionApi: %d, hasReadAPI: %d, hasInsertAPI: %d, canGetSelection: %d, canReadText: %d, canInsertText: %d, canReplaceText: %d, mustBackspaceUsingEvents: %d, clientAppId: %@, client: %@", self.apiComplianceUncertain, self.hasCompliantSelectionApi, self.hasReadApi, self.hasInsertApi, [self canGetSelection], [self canReadText], [self canInsertText], [self canReplaceText], [self mustBackspaceUsingEvents], _clientApplicationId, _client];
}

/** test to see if the API selectedRange functions properly for the text input client  */
-(void) testApiCompliance:(id) client {
  BOOL selectionApiVerified = NO;

  // confirm that the API actually exists (this always seems to return true)
    selectionApiVerified = [client respondsToSelector:@selector(selectedRange)];
  
  // so it exists, now call the API and see if it works as expected
  if (selectionApiVerified) {
    NSRange selectionRange = [client selectedRange];
    NSLog(@"TextCompatibilityCheck testSelectionApi, location = %lu, length = %lu", selectionRange.location, selectionRange.length);

    if (selectionRange.location == NSNotFound) {
      // NSNotFound may just mean that we don't have the focus yet
      // say NO for now, but this may toggle back to YES after the first insertText
      selectionApiVerified = NO;
      self.apiComplianceUncertain = YES;
      NSLog(@"TextCompatibilityCheck checkSelectionApi not compliant but uncertain, range is NSNotFound");
    } else if (selectionRange.location == 0) {
      // location zero may just mean that we are at the beginning of the doc
      // say YES for now, but this may toggle back to NO after the first insertText
      selectionApiVerified = YES;
      self.apiComplianceUncertain = YES;
      NSLog(@"TextCompatibilityCheck checkSelectionApi compliant but uncertain, location = 0");
    } else if (selectionRange.location > 0) {
      // we are confident, based on testing, that selectedRange API does  work
      selectionApiVerified = YES;
      self.apiComplianceUncertain = NO;
      NSLog(@"TextCompatibilityCheck checkSelectionApi compliant and certain, location > 0");
    }
  }
  
  NSLog(@"***testSelectionApi workingSelectionApi for app %@: set to %@", self.clientApplicationId, selectionApiVerified?@"yes":@"no");
  
  self.hasCompliantSelectionApi = selectionApiVerified;
}

/** if apiComplianceUncertain is true, checking the selection after an insert can make it clear  */
-(void) testApiComplianceAfterInsert:(id) client {
  if(self.apiComplianceUncertain) {
    NSRange selectionRange = [client selectedRange];
    NSLog(@"TextCompatibilityCheck testSelectionApiAfterInsert, location = %lu, length = %lu", selectionRange.location, selectionRange.length);

    if (selectionRange.location == NSNotFound) {
      // NO for certain, insertText means we have focus, NSNotFound means that the selection API does not work
      self.hasCompliantSelectionApi = NO;
      self.apiComplianceUncertain = NO;
      NSLog(@"TextCompatibilityCheck testApiComplianceAfterInsert certain, non-compliant, range is NSNotFound");
    } else if (selectionRange.location == 0) {
      // NO for certain, after an insertText we cannot be at location 0
      self.hasCompliantSelectionApi = NO;
      self.apiComplianceUncertain = NO;
      NSLog(@"TextCompatibilityCheck testApiComplianceAfterInsert certain, non-compliant, location = 0");
    } else if (selectionRange.location > 0) {
      // we are confident, based on testing, the selectedRange API does work
      self.hasCompliantSelectionApi = YES;
      self.apiComplianceUncertain = NO;
      NSLog(@"TextCompatibilityCheck checkSelectionApi compliant and certain, location > 0");
    }
    
    NSLog(@"testSelectionApiAfterInsert, self.hasWorkingSelectionApi = %@ for app %@", self.hasCompliantSelectionApi?@"yes":@"no", self.clientApplicationId);
    NSLog(@"testSelectionApiAfterInsert checkClientTextCompatibility: %@", self);
 } else {
    NSLog(@"TextCompatibilityCheck testSelectionApiAfterInsert, compliance is already known");
  }
}

/**
 * Checks if the client app is known to be non-complian, first by checking the hard-coded non-compliant app list
 * and then by checking the user-managed (via user defaults) non-compliant app list
 */
- (BOOL)containedInNoncompliantAppLists:(NSString *)clientAppId {
  BOOL isAppNonCompliant = [self containedInHardCodedNoncompliantAppList:clientAppId];
  if (!isAppNonCompliant) {
    isAppNonCompliant = [self containedInUserManagedNoncompliantAppList:clientAppId];
  }
  
  NSLog(@"containedInNoncompliantAppLists: for app %@: %@", clientAppId, isAppNonCompliant?@"yes":@"no");
  return isAppNonCompliant;
}

/** Check this hard-coded list to see if the application ID is among those
* that are known to not implement selectionRange correctly.
*  This was formerly called the legacy app list, renamed to improve clarity.
*/
- (BOOL)containedInHardCodedNoncompliantAppList:(NSString *)clientAppId {
    BOOL isAppNonCompliant = ([clientAppId isEqual: @"com.github.atom"] ||
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
    NSLog(@"containedInHardCodedNoncompliantAppList: for app %@: %@", clientAppId, isAppNonCompliant?@"yes":@"no");
    return isAppNonCompliant;
  }

/**
 * Returns the list of user-default legacy apps
 */
- (NSArray *)legacyAppsUserDefaults {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    return [userData arrayForKey:kKMLegacyApps];
}

/** Check this user-managed list to see if the application ID is among those
* that are known to not implement selectionRange correctly.
*  This was formerly called the legacy app list, renamed to improve clarity.
*/
- (BOOL)containedInUserManagedNoncompliantAppList:(NSString *)clientAppId {
  BOOL isAppNonCompliant = NO;
  NSArray *legacyAppsUserDefaults = self.legacyAppsUserDefaults;

  if(legacyAppsUserDefaults != nil) {
    isAppNonCompliant = [self isClientAppLegacy:clientAppId fromArray:legacyAppsUserDefaults];
  }
  NSLog(@"containedInUserManagedNoncompliantAppList: for app %@: %@", clientAppId, isAppNonCompliant?@"yes":@"no");

    return isAppNonCompliant;
  }

/**
 * Checks if the client app requires legacy input mode, first by checking the user defaults, if they exist,
 * then, by our hard-coded list.
 */
/*
- (BOOL)isClientAppLegacy:(NSString *)clientAppId {
  BOOL isAppNonCompliant = NO;
  
    NSArray *legacyAppsUserDefaults = self.legacyAppsUserDefaults;

    BOOL result = NO;

    if(legacyAppsUserDefaults != nil) {
        result = [self isClientAppLegacy:clientAppId fromArray:legacyAppsUserDefaults];
    }

    if(!result) {
        // TODO: Pages and Keynote (and possibly lots of other undiscovered apps that are otherwise compliant
        // with Apple's IM framework) have a problem in that if the user selects a different font (or other
        // formatting) and then types a sequence that causes characters to be added to the document and then
        // subsequently replaced, the replacement causes the formatting decision to be forgotten. This can be
        // "fixed" by treating them as legacy apps, but it causes other problems.
        result = ([clientAppId isEqual: @"com.github.atom"] ||
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
            [clientAppId isEqual: @"com.microsoft.VSCode"] ||
            [clientAppId isEqual: @"com.google.Chrome"] ||
            [clientAppId hasPrefix: @"net.java"] ||
            [clientAppId isEqual: @"com.Keyman.test.legacyInput"]
            //||[clientAppId isEqual: @"ro.sync.exml.Oxygen"] - Oxygen has worse problems
        );
    }

    return result;
}
*/

/**
 * Checks array for a list of possible regexes to match a client app id
 */
- (BOOL)isClientAppLegacy:(NSString *)clientAppId fromArray:(NSArray *)legacyApps {
    for(id legacyApp in legacyApps) {
        if(![legacyApp isKindOfClass:[NSString class]]) {
            NSLog(@"isClientAppLegacy:fromArray: LegacyApps user defaults array should contain only strings");
        } else {
            NSError *error = nil;
            NSRange range =  NSMakeRange(0, clientAppId.length);
            
            NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern: (NSString *) legacyApp options: 0 error: &error];
            NSArray *matchesArray = [regex matchesInString:clientAppId options:0 range:range];
            if(matchesArray.count>0) {
              NSLog(@"isClientAppLegacy: found match for legacy app %@: ", clientAppId);
               return YES;
            }
        }
    }

    return NO;
}

-(BOOL) isApiComplianceUncertain {
  return self.apiComplianceUncertain;
}


-(BOOL) canGetSelection {
  return self.hasCompliantSelectionApi;
}

-(BOOL) canReadText {
  // seems simple, but every application tested that returned the selection also successfully read the text using attributedSubstringFromRange
  BOOL canReadText =  self.hasReadApi && self.canGetSelection;
  return canReadText;
}

-(BOOL) canInsertText {
  // all applications tested can insertText (though replacement may not work)
  return self.hasInsertApi;
}

-(BOOL) canReplaceText {
  // testing shows that every application that returns the selection, can also replace text
  return self.hasInsertApi && self.canGetSelection;
}

-(BOOL) mustBackspaceUsingEvents {
  // if we cannot replace text, then we always need to use events to backspace
  return !self.canReplaceText;
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
