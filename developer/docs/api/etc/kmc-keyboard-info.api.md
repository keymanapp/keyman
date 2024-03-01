## API Report File for "@keymanapp/kmc-keyboard-info"

> Do not edit this file. It is a report generated by [API Extractor](https://api-extractor.com/).

```ts

import { CompilerCallbacks } from '@keymanapp/common-types';
import { CompilerEvent } from '@keymanapp/common-types';
import { CompilerOptions } from '@keymanapp/common-types';
import { KeymanCompiler } from '@keymanapp/common-types';
import { KeymanCompilerArtifact } from '@keymanapp/common-types';
import { KeymanCompilerArtifacts } from '@keymanapp/common-types';
import { KeymanCompilerResult } from '@keymanapp/common-types';
import { KmpJsonFile } from '@keymanapp/common-types';

// @public (undocumented)
export class KeyboardInfoCompiler implements KeymanCompiler {
    constructor();
    // Warning: (ae-forgotten-export) The symbol "KeyboardInfoFileLanguageFont" needs to be exported by the entry point index.d.ts
    //
    // (undocumented)
    fontSourceToKeyboardInfoFont(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile, source: string[]): Promise<KeyboardInfoFileLanguageFont>;
    // (undocumented)
    init(callbacks: CompilerCallbacks, options: KeyboardInfoCompilerOptions): Promise<boolean>;
    run(inputFilename: string, outputFilename?: string): Promise<KeyboardInfoCompilerResult>;
    // (undocumented)
    write(artifacts: KeyboardInfoCompilerArtifacts): Promise<boolean>;
}

// @public (undocumented)
export interface KeyboardInfoCompilerArtifacts extends KeymanCompilerArtifacts {
    // (undocumented)
    keyboard_info: KeymanCompilerArtifact;
}

// @public (undocumented)
export class KeyboardInfoCompilerMessages {
    // (undocumented)
    static ERROR_CannotBuildWithoutKmpFile: number;
    // (undocumented)
    static Error_CannotBuildWithoutKmpFile: () => CompilerEvent;
    // (undocumented)
    static ERROR_FileDoesNotExist: number;
    // (undocumented)
    static Error_FileDoesNotExist: (o: {
        filename: string;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_FileIsNotValid: number;
    // (undocumented)
    static Error_FileIsNotValid: (o: {
        filename: string;
        e: any;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_FontFileCannotBeRead: number;
    // (undocumented)
    static Error_FontFileCannotBeRead: (o: {
        filename: string;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_InvalidAuthorEmail: number;
    // (undocumented)
    static Error_InvalidAuthorEmail: (o: {
        email: string;
    }) => CompilerEvent;
    // (undocumented)
    static Error_LicenseFileDoesNotExist: (o: {
        filename: string;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_LicenseFileIsDamaged: number;
    // (undocumented)
    static Error_LicenseFileIsDamaged: (o: {
        filename: string;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_LicenseFileIsMissing: number;
    // (undocumented)
    static ERROR_LicenseIsNotValid: number;
    // (undocumented)
    static Error_LicenseIsNotValid: (o: {
        filename: string;
        message: string;
    }) => CompilerEvent;
    // (undocumented)
    static ERROR_NoLicenseFound: number;
    // (undocumented)
    static Error_NoLicenseFound: () => CompilerEvent;
    // (undocumented)
    static ERROR_OutputValidation: number;
    // (undocumented)
    static Error_OutputValidation: (o: {
        message: any;
    }) => CompilerEvent;
    // (undocumented)
    static FATAL_UnexpectedException: number;
    // (undocumented)
    static Fatal_UnexpectedException: (o: {
        e: any;
    }) => CompilerEvent;
    // (undocumented)
    static HINT_OutputValidation: number;
    // (undocumented)
    static Hint_OutputValidation: (o: {
        message: any;
    }) => CompilerEvent;
    // (undocumented)
    static WARN_MetadataFieldInconsistent: number;
    // (undocumented)
    static Warn_MetadataFieldInconsistent: (o: {
        field: string;
        value: any;
        expected: any;
    }) => CompilerEvent;
    // (undocumented)
    static WARN_OutputValidation: number;
    // (undocumented)
    static Warn_OutputValidation: (o: {
        message: any;
    }) => CompilerEvent;
}

// @public (undocumented)
export interface KeyboardInfoCompilerOptions extends CompilerOptions {
    // (undocumented)
    sources: KeyboardInfoSources;
}

// @public (undocumented)
export interface KeyboardInfoCompilerResult extends KeymanCompilerResult {
    // (undocumented)
    artifacts: KeyboardInfoCompilerArtifacts;
}

// @public (undocumented)
export interface KeyboardInfoSources {
    forPublishing: boolean;
    jsFilename?: string;
    kmpFilename: string;
    kpsFilename: string;
    lastCommitDate?: string;
    sourcePath: string;
}

// (No @packageDocumentation comment for this package)

```