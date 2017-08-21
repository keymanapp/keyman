//
//  KMURLProtocol.m
//  Keyman Engine
//
//  Created by Serkan Kurt on 26/09/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMURLProtocol.h"

@interface KMURLProtocol () <NSURLConnectionDelegate>
@property (nonatomic, strong) NSURLConnection *connection;
@end

@implementation KMURLProtocol

+ (BOOL)canInitWithRequest:(NSURLRequest *)request {
    NSString *mainDoc = [[request mainDocumentURL] lastPathComponent];
    NSString *scheme = [[request URL] scheme];
    if ([mainDoc isEqualToString:@"keyboard.html"] && ![scheme isEqualToString:@"file"]) {
        if ([NSURLProtocol propertyForKey:@"KMURLProtocolHandledKey" inRequest:request]) {
            return NO;
        }
        
        return YES;
    }
    
    return NO;
}

+ (NSURLRequest *)canonicalRequestForRequest:(NSURLRequest *)request {
    return request;
}

+ (BOOL)requestIsCacheEquivalent:(NSURLRequest *)a toRequest:(NSURLRequest *)b {
    return [super requestIsCacheEquivalent:a toRequest:b];
}

- (void)startLoading {
    NSMutableURLRequest *newRequest = [self.request mutableCopy];
    [NSURLProtocol setProperty:@YES forKey:@"KMURLProtocolHandledKey" inRequest:newRequest];
    // Block request
    //self.connection = [NSURLConnection connectionWithRequest:newRequest delegate:self];
}

- (void)stopLoading {
    [self.connection cancel];
    self.connection = nil;
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    [self.client URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    [self.client URLProtocol:self didLoadData:data];
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    [self.client URLProtocolDidFinishLoading:self];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    [self.client URLProtocol:self didFailWithError:error];
}

@end
