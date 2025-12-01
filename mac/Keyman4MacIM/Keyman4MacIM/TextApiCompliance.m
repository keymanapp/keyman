/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * TextApiCompliance.m
 * Keyman
 *
 * Created by Shawn Schantz on 2023-05-05.
 *
 * Check whether the current text input client is compliant with APIs.
 * Compliance is determined by getting the current selection (location and
 * length) with the selectionRange API and evaluating the results. But this is
 * not a surefire indicator. For several apps,  a valid but incorrect selection
 * of {0, 0}, or possibly something else, is returned regardless of the real
 * selection.
 *
 * When a value other than NSNotFound is returned, we can check the selection
 * after a call to the insertText API to see whether it results in a change in
 * the selection. If the selection does change, then we assume the selection API
 * is compliant.
 *
 * For some apps, the selection API returns a resonable value, but any attempt to
 * replace text with a call to insertText do not function properly. There is
 * no way to detect this behavior, so these apps must be hard-coded as
 * non-compliant.  A couple of known apps that behave this way are Brackets
 * (Adobe OS project) and MacVIM
 *
 * We really only have the ability to test the selection API, and if it does not
 * work, then Keyman functionality is limited. Lacking the current location
 * - we cannot pass a range for reading the context
 * - we cannot pass a replacement range when calling insertText The result is
 *   that we are limited in key processing, using only the cached context, and
 *   we can only delete by sending backspace events.
 */

#import "TextApiCompliance.h"
#import <InputMethodKit/InputMethodKit.h>
#import "KMInputMethodAppDelegate.h"
#import "KMLogs.h"
#import "KMSentryHelper.h"

// this is the user managed list of non-compliant apps persisted in User Defaults
NSString *const kKMLegacyApps = @"KMLegacyApps";

@interface TextApiCompliance()

@property (readonly, weak) id client;
@property BOOL complianceUncertain;
@property BOOL hasCompliantSelectionApi;
@property BOOL hasCompliantAttributedSubstringApi;
@property BOOL didValidateAttributedSubstringApi;
@property NSRange initialSelection;

@end

@implementation TextApiCompliance

-(KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId {
  self = [super init];
  if (self) {
    _client = client;
    _clientApplicationId = appId;
    _complianceUncertain = YES;
    _hasCompliantAttributedSubstringApi = NO;
    _didValidateAttributedSubstringApi = NO;
    _initialSelection = NSMakeRange(NSNotFound, NSNotFound);

    NSString *message = [NSString stringWithFormat:@"TextApiCompliance initWithClient, client: %p applicationId: %@", client, appId];
    [KMSentryHelper addDebugBreadCrumb:@"compliance" message:message];
    os_log_debug([KMLogs complianceLog], "%{public}@", message);

    // if we do not have hard-coded noncompliance, then test the client APIs for compliance
    if (appId && [self isAbsentFromNoncompliantAppLists:appId]) {
      [self checkCompliance];
    } else {
      [self markAsNoncompliant];
    }
  }
  return self;
}

- (BOOL)isEqual:(id)other {
  // this is the same object
  if (other == self) {
    return YES;
  }
  // this is an object of another type
  if (!other || ![other isKindOfClass:[self class]]) {
    return NO;
  }
  // both are unique TextApiCompliance objects, compare what matters
  return [self isEqualToObject:other];
}

- (BOOL)isEqualToObject:(TextApiCompliance *)complianceObject {
  if (self == complianceObject) {
    return YES;
  }
  if (![(id)[self client] isEqual:[complianceObject client]]) {
    return NO;
  }
  if (![[self clientApplicationId] isEqual:[complianceObject clientApplicationId]]) {
    return NO;
  }
  return YES;
}

-(BOOL)isMatchingClient:(nullable id) otherClient applicationId:(nullable NSString *)otherAppId {
  BOOL matches = YES;
  if (![(id)[self client] isEqual:otherClient]) {
    os_log_debug([KMLogs complianceLog], "## isMatchingClient = NO, self.client %p, otherClient %p", [self client], otherClient);
    matches = NO;
  }
  if (![[self clientApplicationId] isEqual:otherAppId]) {
    os_log_debug([KMLogs complianceLog], "## isMatchingClient = NO, self.clientApplicationId %{public}@ otherAppId %{public}@", [self clientApplicationId], otherAppId);
    matches = NO;
  }
  
  os_log_debug([KMLogs complianceLog], "## isMatchingClient = %@", matches?@"YES":@"NO");
  return matches;
}

-(NSString *)description
{
  return [NSString stringWithFormat:@"complianceUncertain: %d, hasCompliantSelectionApi: %d, canReadText: %d, canReplaceText: %d, mustBackspaceUsingEvents: %d, clientApplicationId: %@, client: %@", self.complianceUncertain, self.hasCompliantSelectionApi, [self canReadText], [self canReplaceText], [self mustBackspaceUsingEvents], _clientApplicationId, _client];
}

/**
 * For Sentry, create a short description as the tag value is limited to 200 characters.
 * We don't know how long the clientAppId will be, but this should not be well below 200 characters.
 * If the text were to exceed 200 characters, then Sentry would display an error rather than truncating.
 */
-(NSString *)shortSentryTagDescription
{
  return [NSString stringWithFormat:@"uncertain: %@, canGetSelection: %@, canReadText: %@, clientAppId: %@, ", self.complianceUncertain?@"true":@"false", self.hasCompliantSelectionApi?@"true":@"false", [self canReadText]?@"true":@"false",_clientApplicationId];
}

/** test to see if the API selectedRange functions properly for the text input client  */
-(void) checkCompliance {
  // confirm that the API actually exists (always seems to return true unless client is nil)
  self.hasCompliantSelectionApi = [self.client respondsToSelector:@selector(selectedRange)];
  
  if (!self.hasCompliantSelectionApi) {
    self.complianceUncertain = NO;
  }
  else {
    // if API exists, call it and see if it works as expected
    self.initialSelection = [self.client selectedRange];
    os_log_debug([KMLogs complianceLog], "checkCompliance, location = %lu, length = %lu", self.initialSelection.location, self.initialSelection.length);
    [self checkComplianceUsingInitialSelection];
  }
  NSString *message = [NSString stringWithFormat:@"checkCompliance workingSelectionApi for app %@: set to %@", self.clientApplicationId, self.complianceUncertain?@"YES":@"NO"];
  [KMSentryHelper addDebugBreadCrumb:@"compliance" message:message];
  os_log_debug([KMLogs complianceLog], "%{public}@", message);
  [KMSentryHelper addApiComplianceTag:self.shortSentryTagDescription];
}

-(void) checkComplianceUsingInitialSelection {
  if (self.initialSelection.location == NSNotFound) {
    /**
     * NSNotFound may just mean that we don't have the focus yet; say NO for now
     * now, but this may toggle back to YES after the first insertText
     */
    self.hasCompliantSelectionApi = NO;
    self.complianceUncertain = YES;
    os_log_debug([KMLogs complianceLog], "checkComplianceUsingInitialSelection not compliant but uncertain, range is NSNotFound");
  } else if (self.initialSelection.location >= 0) {
    /**
     * location greater than or equal to zero may just mean that the client
     * returns an inaccurate value; say YES for now, but set back to NO if the
     * if the selection is not consistent with the first insert
     */
    self.hasCompliantSelectionApi = YES;
    self.complianceUncertain = YES;
    os_log_debug([KMLogs complianceLog], "checkComplianceUsingInitialSelection compliant but uncertain, location >= 0");
  }
}

/**
 * If complianceUncertain is true, checking the location after an insert can confirm whether the app is compliant.
 * insertedText and deletedText contain the text that Keyman Core instructed us to insert and delete
 * Text that was selected in the client when the key was processed is irrelevant as it does not affect the location.
 */
-(void) checkComplianceAfterInsert:(NSString *)insertedText deleted:(NSString *)deletedText {
  
  // return if compliance is already certain
  if(!self.complianceUncertain) {
    os_log_debug([KMLogs complianceLog], "checkComplianceAfterInsert, compliance is already known");
    return;
  }
  
  // check whether the location has changed as expected
  NSRange selectionRange = [self.client selectedRange];
  os_log_debug([KMLogs complianceLog], "  selectionRange: %{public}@", NSStringFromRange(selectionRange));
  if ([self validateNewLocation:selectionRange.location delete:deletedText]) {
    BOOL changeExpected = [self isLocationChangeExpectedOnInsert:deletedText insert:insertedText];
    BOOL locationChanged = [self hasLocationChanged:selectionRange];
    [self validateLocationChange:changeExpected hasLocationChanged:locationChanged];
  }
  
  // only validate ability to read if we know we have ability to get selection
  if (self.hasCompliantSelectionApi) {
    [self validateCanReadInsertedText:insertedText location:selectionRange.location];
  }
  
  NSString *message = [NSString stringWithFormat:@"checkComplianceAfterInsert, self.canReadText = %@, self.hasWorkingSelectionApi = %@ for app %@", self.canReadText?@"YES":@"NO", self.hasCompliantSelectionApi?@"YES":@"NO", self.clientApplicationId];
  os_log_debug([KMLogs complianceLog], "%{public}@", message);
  [KMSentryHelper addDebugBreadCrumb:@"compliance" message:message];
  [KMSentryHelper addApiComplianceTag:self.shortSentryTagDescription];
}

/**
 * Confirm that we can get the string that ends at the current location,
 * and the end of the string matches the string that  was just inserted
 */
- (void)validateCanReadInsertedText:(NSString *)insertedText location: (NSUInteger)newLocation {

  NSString *context = [self getContext:newLocation];
  
  if (context && context.length > 0) {
    if ([context hasSuffix:insertedText]) {
      self.hasCompliantAttributedSubstringApi = YES;
      os_log_debug([KMLogs complianceLog], "  validateCanReadInsertedText passed, context matches insertedText '%{public}@'", insertedText);
    } else {
      self.hasCompliantAttributedSubstringApi = NO;
      os_log_debug([KMLogs complianceLog], "  validateCanReadInsertedText failed, context does not have suffix of insertedText '%{public}@'", insertedText);
    }
  } else {
    self.hasCompliantAttributedSubstringApi = NO;
    os_log_debug([KMLogs complianceLog], "  validateCanReadInsertedText failed, attributedSubstringFromRange returned nil or empty string");
  }

  self.complianceUncertain = NO;
  self.didValidateAttributedSubstringApi = YES;
}

// TODO: getContext and calculateContextRange roughly duplicates code in KMInputMethodHandler
// These should be moved to a class that wraps the text input client and its three APIs

/**
 * Get the string that ends with the current location using `attributedSubstringFromRange`
 */
- (NSString *)getContext: (NSUInteger)newLocation {
  NSAttributedString *attributedString = nil;
  NSRange contextRange = [self calculateContextRange: newLocation];
  
  attributedString = [self.client attributedSubstringFromRange:contextRange];
  os_log_debug([KMLogs complianceLog], "getContext length %lu character range: %{public}@: client: %p", contextRange.length, NSStringFromRange(contextRange), self.client);
  
  if (attributedString) {
    return attributedString.string;
  } else {
    return nil;
  }
}

/**
 * Calculate the range which bounds the context that we want to read, up to 80 characters.
 */
- (NSRange) calculateContextRange: (NSUInteger)newLocation {
  const NSInteger kMaxContextLength = 80;
  
  NSUInteger contextLength = MIN(kMaxContextLength, newLocation);
  NSUInteger contextStart = newLocation - contextLength;

  NSRange contextRange = NSMakeRange(contextStart, contextLength);

  return contextRange;
}

/**
 * Inspect the current location returned by `selectionRange` to see whether it is valid.
 */
- (BOOL)validateNewLocation:(NSUInteger)location delete:(NSString *)textToDelete  {
  BOOL validLocation = NO;
  
  if (location == NSNotFound) {
    // invalid location: insertText means we have focus, NSNotFound means selection API not functioning
    self.hasCompliantSelectionApi = NO;
    self.complianceUncertain = NO;
    os_log_debug([KMLogs complianceLog], "validateNewLocation = NO, location is NSNotFound");
  } else if ((location == 0) && (textToDelete.length > 0)) {
    // invalid location: cannot have text to delete at location zero
    self.hasCompliantSelectionApi = NO;
    self.complianceUncertain = NO;
    os_log_debug([KMLogs complianceLog], "validateNewLocation = NO, location is zero with textToDelete.length > 0");
  } else {
    // location is valid, but do not know if it is compliant yet
    validLocation = YES;
    os_log_debug([KMLogs complianceLog], "validateNewLocation = YES, newLocation = %lu, oldLocation = %lu", (unsigned long)location, (unsigned long)self.initialSelection.location);
  }
  return validLocation;
}

/**
 * Compare the the new location to the previous location and validate that it changed as expected after the latest insert.
 */
- (void) validateLocationChange:(BOOL) changeExpected hasLocationChanged:(BOOL) locationChanged {
  
  if (changeExpected == locationChanged) {
    // YES for certain, the location is where we expect it
    self.hasCompliantSelectionApi = YES;
    self.complianceUncertain = NO;
    os_log_debug([KMLogs complianceLog], "validateLocationChange compliant, locationChanged = %{public}@, changeExpected = %{public}@", locationChanged?@"YES":@"NO", changeExpected?@"YES":@"NO");
  } else if (changeExpected != locationChanged) {
    // NO for certain, when the selection is unchanged after an insert
    self.hasCompliantSelectionApi = NO;
    self.complianceUncertain = NO;
    os_log_debug([KMLogs complianceLog], "validateLocationChange non-compliant, locationChanged = %{public}@, changeExpected = %{public}@", locationChanged?@"YES":@"NO", changeExpected?@"YES":@"NO");
  }
}

- (BOOL)isLocationChangeExpectedOnInsert:(NSString *)textToDelete insert:(NSString *)textToInsert {
  BOOL changeExpected = textToInsert.length != textToDelete.length;
  os_log_debug([KMLogs complianceLog], "isLocationChangeExpected, changeExpected = %{public}@", changeExpected?@"YES":@"NO");
  
  return changeExpected;
}

- (BOOL)hasLocationChanged:(NSRange)newSelection {
  NSUInteger newLocation = newSelection.location;
  NSUInteger oldLocation = self.initialSelection.location;
  BOOL locationChanged = newLocation != oldLocation;
  os_log_debug([KMLogs complianceLog], "hasLocationChanged: %{public}@, new location: %lu, selection length: %lu", locationChanged?@"YES":@"NO", newSelection.location, newSelection.length);
  return locationChanged;
}

/**
 * Check against the hard-coded non-compliant app list and the user-managed (via user defaults) non-compliant app list.
 * If the specified application ID is not contained in non-compliant app lists, return true
 */
- (BOOL)isAbsentFromNoncompliantAppLists:(NSString *)clientAppId {
  BOOL absent = ![self containedInHardCodedNoncompliantAppList:clientAppId];
  if (absent) {
    absent = ![self containedInUserManagedNoncompliantAppList:clientAppId];
  }
  
  os_log_info([KMLogs complianceLog], "isAbsentFromNoncompliantAppLists: for app %{public}@: %{public}@", clientAppId, absent?@"YES":@"NO");
  
  return absent;
}

-(void)markAsNoncompliant {
  self.complianceUncertain = NO;
  self.hasCompliantSelectionApi = NO;
  self.hasCompliantAttributedSubstringApi = NO;
  self.didValidateAttributedSubstringApi = YES;
}

/** Check this hard-coded list to see if the application ID is among those
 * that are known to not implement selectionRange correctly.
 *  This was formerly called the legacy app list, renamed to improve clarity.
 */
- (BOOL)containedInHardCodedNoncompliantAppList:(NSString *)clientAppId {
  BOOL isAppNonCompliant = (//[clientAppId isEqual: @"com.github.atom"] ||
                            // 2023-12-19[sgs] Atom is now automatically detected as non-compliant
                            [clientAppId isEqual: @"com.collabora.libreoffice-free"] ||
                            [clientAppId isEqual: @"org.libreoffice.script"] ||
                            [clientAppId isEqual: @"org.vim.MacVim"] ||
                            [clientAppId isEqual: @"io.brackets.appshell"] ||
                            [clientAppId isEqual: @"com.axosoft.gitkraken"] ||
                            [clientAppId isEqual: @"org.sil.app.builder.scripture.ScriptureAppBuilder"] ||
                            [clientAppId isEqual: @"org.sil.app.builder.reading.ReadingAppBuilder"] ||
                            [clientAppId isEqual: @"org.sil.app.builder.dictionary.DictionaryAppBuilder"] ||
                            [clientAppId isEqual: @"com.microsoft.Word"] ||
                            // Microsoft Word removed on 2020-11-24 but added back on 2025-10-10 for issue #14160
                            [clientAppId isEqual: @"org.openoffice.script"] ||
                            // 2023-12-13[sgs]: Adobe apps automatically detected as non-compliant
                            //[clientAppId isEqual: @"com.adobe.illustrator"] ||
                            //[clientAppId isEqual: @"com.adobe.InDesign"] ||
                            //[clientAppId isEqual: @"com.adobe.Photoshop"] ||
                            //[clientAppId isEqual: @"com.adobe.AfterEffects"] ||
                            //[clientAppId isEqual: @"com.microsoft.VSCode"] || // 2023-05-29[sgs]: Works with 1.78.2, disable legacy by default
                            [clientAppId isEqual: @"com.google.Chrome"] ||
                            // 2023-12-13[sgs]: must hard-code for Chrome because Google Docs returns relative but incorrect location so no way to auto-detect
                            [clientAppId hasPrefix: @"net.java"] ||
                            [clientAppId isEqual: @"com.Keyman.test.legacyInput"]
                            /*||[clientAppId isEqual: @"ro.sync.exml.Oxygen"] - Oxygen has worse problems */
                            );
  
  os_log_debug([KMLogs complianceLog], "containedInHardCodedNoncompliantAppList: for app %{public}@: %{public}@", clientAppId, isAppNonCompliant?@"yes":@"no");
  return isAppNonCompliant;
}

/**
 * Returns the list of application IDs for non-compliant apps
 */
- (NSArray *)noncompliantAppsUserDefaults {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData arrayForKey:kKMLegacyApps];
}

/**
 * Check this user-managed list to see if the application ID is among those known to be non-compliant.
 */
- (BOOL)containedInUserManagedNoncompliantAppList:(NSString *)clientAppId {
  BOOL isAppNonCompliant = NO;
  NSArray *legacyAppsUserDefaults = [self noncompliantAppsUserDefaults];
  
  if(legacyAppsUserDefaults != nil) {
    isAppNonCompliant = [self arrayContainsApplicationId:clientAppId fromArray:legacyAppsUserDefaults];
  }
  os_log_debug([KMLogs complianceLog], "containedInUserManagedNoncompliantAppList: for app %{public}@: %{public}@", clientAppId, isAppNonCompliant?@"yes":@"no");
  return isAppNonCompliant;
}

/**
 * Checks array for a list of possible regexes to match a client app id
 */
- (BOOL)arrayContainsApplicationId:(NSString *)applicationId fromArray:(NSArray *)applicationArray {
  for(id appId in applicationArray) {
    if(![appId isKindOfClass:[NSString class]]) {
      // always log this: bad data in UserDefaults
      os_log_error([KMLogs complianceLog], "arrayContainsApplicationId:fromArray: LegacyApps user defaults array should contain only strings");
    } else {
      NSError *error = nil;
      NSRange range =  NSMakeRange(0, applicationId.length);
      
      NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern: (NSString *) appId options: 0 error: &error];
      NSArray *matchesArray = [regex matchesInString:applicationId options:0 range:range];
      if(matchesArray.count>0) {
        os_log_debug([KMLogs complianceLog], "arrayContainsApplicationId: found match for application ID %{public}@: ", applicationId);
        return YES;
      }
    }
  }
  
  return NO;
}

-(BOOL) isComplianceUncertain {
  return self.complianceUncertain;
}

/**
 * Returns true if we have a compliant `selectionRange` API and either
 * 1) we have not yet validated the `attributedSubstringFromRange` API, or
 * 2) it was found to be compliant (when validated after the first insert)
 *
 * What this means is that if getting the selection appears to work ok, then we assume that
 * we can also read text -- unless we attempt to read it and it returns something unexpected.
 */
-(BOOL) canReadText {
  BOOL hasReadApi = [self.client respondsToSelector:@selector(attributedSubstringFromRange:)];
  return hasReadApi && self.hasCompliantSelectionApi &&
    (!self.didValidateAttributedSubstringApi || self.hasCompliantAttributedSubstringApi);
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
