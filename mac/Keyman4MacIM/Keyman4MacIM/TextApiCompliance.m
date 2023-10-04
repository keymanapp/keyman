/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * TextApiCompliance.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-05.
 * 
 * Check whether the current text input client is compliant with APIs.
 * Compliance is determined by getting the current selection (location and length) with
 * the selectionRange API and evaluating the results.
 * But this is not a surefire indicator. For several apps,  a valid but incorrect selection of
 * {0, 0}, or possibly something else, is returned regardless of the real selection.
 *
 * When {0, 0} is returned, then we can check to see if a later call to the insertText API
 * results in a change in the location. It always should, because there is no text to
 * replace if the client truly is at location 0.
 * If the location does change, then we assume the selection API is compliant.
 *
 * Even worse for some apps, the selection API returns a non-zero location, but attempts
 * to replace text during a call to insertText do not function properly. There is no way to
 * detect this behavior, so these apps must be hard-coded as non-compliant.  A couple
 * of known apps that behave this way are Brackets (Adobe OS project) and MacVIM
 *
 * We really only have the ability to test the selection API, and if it does not work, then
 * Keyman functionality is limited. Lacking the current location
 * - we cannot pass a range for reading the context
 * - we cannot pass a replacement range when calling insertText
 * The result is that we are limited in key processing, using only locally cached context, and
 * we can only delete by sending backspace events.
 */

#import "TextApiCompliance.h"
#import <InputMethodKit/InputMethodKit.h>

NSString *const kKMLegacyApps = @"KMLegacyApps";

@interface TextApiCompliance()

@property (readonly) id client;
@property BOOL complianceUncertain;
@property BOOL hasCompliantSelectionApi;

@end

@implementation TextApiCompliance

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId {
  self = [super init];
  if (self) {
    _client = client;
    _clientApplicationId = appId;
    _complianceUncertain = YES;
    
    // if we do not have hard-coded noncompliance, then test the app
    if (![self applyNoncompliantAppLists:appId]) {
      [self testCompliance:client];
    }
  }
  return self;
}

-(NSString *)description
{
return [NSString stringWithFormat:@"complianceUncertain: %d, hasCompliantSelectionApi: %d, canReadText: %d, canReplaceText: %d, mustBackspaceUsingEvents: %d, clientAppId: %@, client: %@", self.complianceUncertain, self.hasCompliantSelectionApi, [self canReadText], [self canReplaceText], [self mustBackspaceUsingEvents], _clientApplicationId, _client];
}

/** test to see if the API selectedRange functions properly for the text input client  */
-(void) testCompliance:(id) client {
  BOOL selectionApiVerified = NO;

  // confirm that the API actually exists (this always seems to return true)
  selectionApiVerified = [client respondsToSelector:@selector(selectedRange)];
  
  if (!selectionApiVerified) {
    self.complianceUncertain = NO;
    self.hasCompliantSelectionApi = NO;
  }
  else {
    // if API exists, call it and see if it works as expected
    NSRange selectionRange = [client selectedRange];
    NSLog(@"TextApiCompliance testSelectionApi, location = %lu, length = %lu", selectionRange.location, selectionRange.length);
    
    if (selectionRange.location == NSNotFound) {
      // NSNotFound may just mean that we don't have the focus yet
      // say NO for now, but this may toggle back to YES after the first insertText
      selectionApiVerified = NO;
      self.complianceUncertain = YES;
      NSLog(@"TextApiCompliance checkSelectionApi not compliant but uncertain, range is NSNotFound");
    } else if (selectionRange.location == 0) {
      // location zero may just mean that we are at the beginning of the doc
      // say YES for now, but this may toggle back to NO after the first insertText
      selectionApiVerified = YES;
      self.complianceUncertain = YES;
      NSLog(@"TextApiCompliance checkSelectionApi compliant but uncertain, location = 0");
    } else if (selectionRange.location > 0) {
      // we are confident, based on testing, that selectedRange API does  work
      selectionApiVerified = YES;
      self.complianceUncertain = NO;
      NSLog(@"TextApiCompliance checkSelectionApi compliant and certain, location > 0");
    }
  }
  
  NSLog(@"***testSelectionApi workingSelectionApi for app %@: set to %@", self.clientApplicationId, selectionApiVerified?@"yes":@"no");
  
  self.hasCompliantSelectionApi = selectionApiVerified;
}

/** if complianceUncertain is true, checking the selection after an insert can make it clear  */
-(void) testComplianceAfterInsert:(id) client {
  if(self.complianceUncertain) {
    NSRange selectionRange = [client selectedRange];
    NSLog(@"TextApiCompliance testSelectionApiAfterInsert, location = %lu, length = %lu", selectionRange.location, selectionRange.length);

    if (selectionRange.location == NSNotFound) {
      // NO for certain, insertText means we have focus, NSNotFound means that the selection API does not work
      self.hasCompliantSelectionApi = NO;
      self.complianceUncertain = NO;
      NSLog(@"TextApiCompliance testComplianceAfterInsert certain, non-compliant, range is NSNotFound");
    } else if (selectionRange.location == 0) {
      // NO for certain, after an insertText we cannot be at location 0
      self.hasCompliantSelectionApi = NO;
      self.complianceUncertain = NO;
      NSLog(@"TextApiCompliance testComplianceAfterInsert certain, non-compliant, location = 0");
    } else if (selectionRange.location > 0) {
      // we are confident, based on testing, the selectedRange API does work
      self.hasCompliantSelectionApi = YES;
      self.complianceUncertain = NO;
      NSLog(@"TextApiCompliance checkSelectionApi compliant and certain, location > 0");
    }
    
    NSLog(@"testSelectionApiAfterInsert, self.hasWorkingSelectionApi = %@ for app %@", self.hasCompliantSelectionApi?@"yes":@"no", self.clientApplicationId);
    NSLog(@"testSelectionApiAfterInsert TextApiCompliance: %@", self);
 } else {
    NSLog(@"TextApiCompliance testSelectionApiAfterInsert, compliance is already known");
  }
}

/**
 * Apply the hard-coded non-compliant app list and the user-managed (via user defaults) non-compliant app list.
 * If true, mark the app as non-compliant and certain (not complianceUncertain).
 */
- (BOOL)applyNoncompliantAppLists:(NSString *)clientAppId {
  BOOL isAppNonCompliant = [self containedInHardCodedNoncompliantAppList:clientAppId];
  if (!isAppNonCompliant) {
    isAppNonCompliant = [self containedInUserManagedNoncompliantAppList:clientAppId];
  }
  
  NSLog(@"containedInNoncompliantAppLists: for app %@: %@", clientAppId, isAppNonCompliant?@"yes":@"no");
  
  if (isAppNonCompliant) {
    self.complianceUncertain = NO;
    self.hasCompliantSelectionApi = NO;
  }
  
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
                     [clientAppId isEqual: @"org.vim.MacVim"] ||
                     [clientAppId isEqual: @"io.brackets.appshell"] ||
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

-(BOOL) isComplianceUncertain {
  return self.complianceUncertain;
}

-(BOOL) canReadText {
  // seems simple, but every application tested that returned the selection also successfully read the text using attributedSubstringFromRange
  
  BOOL hasReadApi = [self.client respondsToSelector:@selector(attributedSubstringFromRange:)];
  return hasReadApi && self.hasCompliantSelectionApi;
}

-(BOOL) canReplaceText {
  // testing shows that every application that returns the selection, can also replace text
  return self.hasCompliantSelectionApi;
}

-(BOOL) mustBackspaceUsingEvents {
  // if we cannot replace text, then we always need to use events to backspace
  return !self.canReplaceText;
}

@end
