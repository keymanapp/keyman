import { type KeyboardStub } from './keyboardLoaderBase.js';

export interface KeyboardLoadErrorBuilder {
    scriptError(err?: Error): void;
    missingError(err: Error): void;
}

export class KeyboardScriptError extends Error {
    public readonly cause;

    constructor(msg: string, cause?: Error) {
        super(msg);
        this.cause = cause;
    }
}

export class KeyboardMissingError extends Error {
    public readonly cause;

    constructor(msg: string, cause?: Error) {
        super(msg);
        this.cause = cause;
    }
}

export class UriBasedErrorBuilder implements KeyboardLoadErrorBuilder {
    readonly uri: string;

    constructor(uri: string) {
        this.uri = uri;
    }

    missingError(err: Error) {
        const msg = `Cannot find the keyboard at ${this.uri}.`;
        return new KeyboardMissingError(msg, err);
    }

    scriptError(err: Error) {
        const msg = `Error registering the keyboard script at ${this.uri}; it may contain an error.`;
        return new KeyboardScriptError(msg, err);
    }
}

export class StubBasedErrorBuilder implements KeyboardLoadErrorBuilder {
    readonly stub: KeyboardStub;

    constructor(stub: KeyboardStub) {
        this.stub = stub;
    }

    missingError(err: Error) {
        const stub = this.stub;
        const msg = `Cannot find the ${stub.name} keyboard for ${stub.langName} at ${stub.filename}.`;
        return new KeyboardMissingError(msg, err);
    }

    scriptError(err: Error) {
        const stub = this.stub;
        const msg = `Error registering the ${stub.name} keyboard for ${stub.langName}; keyboard script at ${stub.filename} may contain an error.`;
        return new KeyboardScriptError(msg, err);
    }
}

