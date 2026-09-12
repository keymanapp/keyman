/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyConsent.h
 * Keyman4MacIM
 * 
 * Created by Shawn Schantz on 2022-09-22.
 * 
 */

#import <Foundation/Foundation.h>
#import "PrivacyWindowController.h"

NS_ASSUME_NONNULL_BEGIN

// command strings passed from Keyman Configuration
extern NSString *kMigrateCommand;
extern NSString *kAccessCommand;
extern NSString *kCheckCommand;

// notification messages sent to Keyman Configuration
extern NSString *kAcessibilityPermissionGrantedMessage;
extern NSString *kAcessibilityPermissionNotGrantedMessage;

int requestAccessibility(void);
int checkAccessibility(void);

@interface PrivacyConsent : NSObject
@property (nonatomic, strong) PrivacyWindowController *privacyDialog;
@property (nonatomic, copy, nullable) void (^completionHandler)(void);
+ (PrivacyConsent *)shared;
- (BOOL)checkAccessibility;
- (BOOL)checkPostEventAccess;
- (void)requestPrivacyAccess:(void (^)(void))withCompletionHandler;
- (void)requestPrivacyAccessForKeyman19:(void (^)(void))withCompletionHandler;
@end

NS_ASSUME_NONNULL_END
