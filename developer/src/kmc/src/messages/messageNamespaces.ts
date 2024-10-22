import { CommonTypesMessages, CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { AnalyzerMessages } from '@keymanapp/kmc-analyze';
import { CopierMessages } from '@keymanapp/kmc-copy';
import { GeneratorMessages } from '@keymanapp/kmc-generate';
import { KeyboardInfoCompilerMessages } from '@keymanapp/kmc-keyboard-info';
import { KmnCompilerMessages, KmwCompilerMessages } from '@keymanapp/kmc-kmn';
import { LdmlCompilerMessages } from '@keymanapp/kmc-ldml';
import { ModelCompilerMessages } from '@keymanapp/kmc-model';
import { ModelInfoCompilerMessages } from '@keymanapp/kmc-model-info';
import { PackageCompilerMessages } from '@keymanapp/kmc-package';
import { InfrastructureMessages } from './infrastructureMessages.js';

// Maps every compiler error namespace to the corresponding implementation
const messageNamespaces: Record<CompilerErrorNamespace, any> = {
  [CompilerErrorNamespace.LdmlKeyboardCompiler]: LdmlCompilerMessages,
  [CompilerErrorNamespace.CommonTypes]: CommonTypesMessages,
  [CompilerErrorNamespace.KmnCompiler]: KmnCompilerMessages,
  [CompilerErrorNamespace.ModelCompiler]: ModelCompilerMessages,
  [CompilerErrorNamespace.PackageCompiler]: PackageCompilerMessages,
  [CompilerErrorNamespace.Infrastructure]: InfrastructureMessages,
  [CompilerErrorNamespace.Analyzer]: AnalyzerMessages,
  [CompilerErrorNamespace.KmwCompiler]: KmwCompilerMessages,
  [CompilerErrorNamespace.ModelInfoCompiler]: ModelInfoCompilerMessages,
  [CompilerErrorNamespace.KeyboardInfoCompiler]: KeyboardInfoCompilerMessages,
  [CompilerErrorNamespace.Copier]: CopierMessages,
  [CompilerErrorNamespace.Generator]: GeneratorMessages,
};

// This works around pain points in enumerating enum members in Typescript
// ref https://www.totaltypescript.com/iterate-over-object-keys-in-typescript
export const messageNamespaceKeys = Object.keys(messageNamespaces).map(v => Number.parseInt(v) as CompilerErrorNamespace);

export type CompilerMessageSource = {
  module: string;
  class: any;
};

// TODO: consolidate with messageNamespaces above
export const messageSources: Record<CompilerErrorNamespace, CompilerMessageSource> = {
  [CompilerErrorNamespace.LdmlKeyboardCompiler]: { module: 'kmc-ldml',          class: LdmlCompilerMessages },
  [CompilerErrorNamespace.CommonTypes]:          { module: 'common-types',      class: CommonTypesMessages },
  [CompilerErrorNamespace.KmnCompiler]:          { module: 'kmc-kmn',           class: KmnCompilerMessages },
  [CompilerErrorNamespace.ModelCompiler]:        { module: 'kmc-model',         class: ModelCompilerMessages },
  [CompilerErrorNamespace.PackageCompiler]:      { module: 'kmc-package',       class: PackageCompilerMessages },
  [CompilerErrorNamespace.Infrastructure]:       { module: 'kmc',               class: InfrastructureMessages },
  [CompilerErrorNamespace.Analyzer]:             { module: 'kmc-analyze',       class: AnalyzerMessages },
  [CompilerErrorNamespace.Copier]:               { module: 'kmc-copy',          class: CopierMessages },
  [CompilerErrorNamespace.Generator]:            { module: 'kmc-generate',      class: GeneratorMessages },
  [CompilerErrorNamespace.KmwCompiler]:          { module: 'kmc-kmn',           class: KmwCompilerMessages },
  [CompilerErrorNamespace.ModelInfoCompiler]:    { module: 'kmc-model-info',    class: ModelInfoCompilerMessages },
  [CompilerErrorNamespace.KeyboardInfoCompiler]: { module: 'kmc-keyboard-info', class: KeyboardInfoCompilerMessages },
};