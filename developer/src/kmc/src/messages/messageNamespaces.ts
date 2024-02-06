import { CommonTypesMessages, CompilerErrorNamespace } from '@keymanapp/common-types';
import { AnalyzerMessages } from '@keymanapp/kmc-analyze';
import { KeyboardInfoCompilerMessages } from '@keymanapp/kmc-keyboard-info';
import { KmnCompilerMessages, KmwCompilerMessages } from '@keymanapp/kmc-kmn';
import { LdmlKeyboardCompilerMessages } from '@keymanapp/kmc-ldml';
import { ModelCompilerMessages } from '@keymanapp/kmc-model';
import { ModelInfoCompilerMessages } from '@keymanapp/kmc-model-info';
import { PackageCompilerMessages } from '@keymanapp/kmc-package';
import { InfrastructureMessages } from './infrastructureMessages.js';

// Maps every compiler error namespace to the corresponding implementation
export const messageNamespaces: Record<CompilerErrorNamespace, any> = {
  [CompilerErrorNamespace.LdmlKeyboardCompiler]: LdmlKeyboardCompilerMessages,
  [CompilerErrorNamespace.CommonTypes]: CommonTypesMessages,
  [CompilerErrorNamespace.KmnCompiler]: KmnCompilerMessages,
  [CompilerErrorNamespace.ModelCompiler]: ModelCompilerMessages,
  [CompilerErrorNamespace.PackageCompiler]: PackageCompilerMessages,
  [CompilerErrorNamespace.Infrastructure]: InfrastructureMessages,
  [CompilerErrorNamespace.Analyzer]: AnalyzerMessages,
  [CompilerErrorNamespace.KmwCompiler]: KmwCompilerMessages,
  [CompilerErrorNamespace.ModelInfoCompiler]: ModelInfoCompilerMessages,
  [CompilerErrorNamespace.KeyboardInfoCompiler]: KeyboardInfoCompilerMessages,
};

// This works around pain points in enumerating enum members in Typescript
// ref https://www.totaltypescript.com/iterate-over-object-keys-in-typescript
export const messageNamespaceKeys = Object.keys(messageNamespaces).map(v => Number.parseInt(v));
