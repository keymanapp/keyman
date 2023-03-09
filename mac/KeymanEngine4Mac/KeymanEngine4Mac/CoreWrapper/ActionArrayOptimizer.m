/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * ActionArrayOptimizer.m
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2023-02-23.
 * 
 * Manipulates an array of CoreAction objects created in response to the processing
 * of a key and reduces them to the minimal representation. For example, a character
 * that causes a re-ordering of the context may cause the character to be deleted.
 * Instead of emitting it twice and deleting it once, just emit it once.
 */

#import "ActionArrayOptimizer.h"

@implementation ActionArrayOptimizer

@end
