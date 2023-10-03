/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * TextCompatibilityCheck.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-05.
 * 
 * Description...
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface TextCompatibilityCheck : NSObject
@property (readonly) NSString *clientApplicationId;

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId;
-(void)testApiCompliance:(id) client;
-(void)testApiComplianceAfterInsert:(id) client;
-(BOOL)isApiComplianceUncertain;
-(BOOL)canReadText;
-(BOOL)mustBackspaceUsingEvents;
@end

NS_ASSUME_NONNULL_END
