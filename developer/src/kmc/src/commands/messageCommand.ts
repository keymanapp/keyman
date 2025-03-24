import * as fs from 'fs';
import * as path from 'path';

import { Command, Option } from 'commander';
import { escapeMarkdownChar, KeymanUrls, CompilerBaseOptions, CompilerCallbacks, CompilerError, CompilerErrorNamespace, CompilerEvent, dedentCompilerMessageDetail } from '@keymanapp/developer-utils';
import { CompilerMessageSource, messageNamespaceKeys, messageSources } from '../messages/messageNamespaces.js';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { exitProcess } from '../util/sysexits.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { CompilerMessageDetail, findMessageDetails, findMessagesById, getMessageIdentifiersSorted } from '../util/extendedCompilerOptions.js';

type MessageFormat = 'text'|'markdown'|'json';

interface MessageOptions extends CompilerBaseOptions {
  /**
   * Output format
   */
  format?: MessageFormat;
  /**
   * Output path, which must exist
   */
  outPath?: string;
  /**
   * Emit descriptions for all messages
   */
  allMessages?: boolean;
};

export function declareMessage(program: Command) {
  program
    .command('message [messages...]')
    .description(`Describe one or more compiler messages. Note: Markdown format is always written to files on disk.`)
    .addHelpText('after', `
Message identifiers can be:
 * numeric, e.g. "KM07006" or "7006", or
 * [namespace.]id, substring id supported; e.g. "kmc-kmn.INFO_MinimumEngineVersion" or "kmc-kmn." or "INFO_Min"`)
    .addOption(new Option('-f, --format <format>', 'Output format').choices(['text', 'markdown', 'json']).default('text'))
    .option('-o, --out-path <out-path>', 'Output path for Markdown files; output filename for text and json formats')
    .option('-a, --all-messages', 'Emit descriptions for all messages (text, json)')
    .action(messageCommand);
}

async function messageCommand(messages: string[], _options: any, commander: any) {
  const commanderOptions = commander.optsWithGlobals();
  const options = initialize(commanderOptions);
  if(!options) {
    await exitProcess(1);
  }
  const callbacks = new NodeCompilerCallbacks(options);

  let result: boolean = false;

  if(options.format == 'markdown') {
    result = messageCommandMarkdown(messages, options, callbacks);
  } else { // json or text format
    if(messages.length == 0 && !options.allMessages) {
      console.error('Must specify at least one message code or -a for all messages');
      callbacks.reportMessage(InfrastructureMessages.Error_MustSpecifyMessageCode())
      await exitProcess(1);
    }

    const messageDetails = messages.length
      ? messages.flatMap(message => translateMessageInputToCode(message, callbacks))
      : allMessageDetails();

    if(callbacks.messageCount > 0) {
      await exitProcess(1);
    }

    let text: string = null;

    if(options.format == 'json') {
      const data = getMessagesAsArrayForJson(messageDetails);
      if(data) {
        text = JSON.stringify(data, null, 2);
      }
    } else {
      text = getMessagesAsText(messageDetails);
    }

    result = text != null;
    if(result) {
      if(options.outPath) {
        fs.writeFileSync(options.outPath, text, 'utf-8');
      } else {
        process.stdout.write(text);
      }
    }
  }

  if(!result) {
    await exitProcess(1);
  }
}

// We have a redirect pattern for kmn.sh/km<#####> to
// the corresponding compiler message reference document in
// help.keyman.com/developer/latest-version/reference/errors/km<#####>
const helpUrl = (code:any) => KeymanUrls.COMPILER_ERROR_CODE(CompilerError.formatCode(code).toLowerCase());
const getModuleName = (ms: CompilerMessageSource) => `${ms.module}.${ms.class.name}`;

function parseMessageIdentifier(message: string, callbacks: CompilerCallbacks): { namespace?: CompilerErrorNamespace, id?: string } {
  const parts = message.split('.', 2);
  if(parts.length == 1) {
    // searching all namespaces
    return { id: parts[0] };
  }

  // searching one namespace
  const namespace = messageNamespaceKeys.find(ns => messageSources[ns].module == parts[0].toLowerCase());
  if(!namespace) {
    return { };
  }
  return { namespace, id: parts[1] };
}

function translateMessageInputToCode(message: string, callbacks: CompilerCallbacks): CompilerMessageDetail[] {
  const pattern = /^(KM)?([0-9a-f]+)$/i;
  const result = message.match(pattern);
  if(!result) {
    const { namespace, id } = parseMessageIdentifier(message.toLowerCase(), callbacks);
    if(!namespace && !id) {
      callbacks.reportMessage(InfrastructureMessages.Error_MessageNamespaceNameNotFound({message}));
      return null;
    }

    // We assume that this is a INFO, HINT, ERROR, etc message, and do a substring search
    const items = findMessagesById(namespace, id);
    if(!items.length) {
      callbacks.reportMessage(InfrastructureMessages.Error_UnrecognizedMessageCode({message}));
      return null;
    }

    return items;
  }

  const code = Number.parseInt(result[2], 16);
  return [findMessageDetails(code, callbacks)];
}

function initialize(options: any): MessageOptions {
  // We don't want to rename command line options to match the precise
  // properties that we have in CompilerOptions, but nor do we want to rename
  // CompilerOptions properties...
  return {
    // CompilerBaseOptions
    logLevel: options.logLevel,
    logFormat: options.logFormat,
    color: options.color,
    // MessageOptions
    format: options.format ?? 'text',
    outPath: options.outPath
  }
}

function allMessageDetails(): CompilerMessageDetail[] {
  let result: CompilerMessageDetail[] = [];
  messageNamespaceKeys.forEach((namespace: CompilerErrorNamespace) => {
    const ms = messageSources[namespace] as CompilerMessageSource;

    const ids = getMessageIdentifiersSorted(ms.class);
    for(const id of ids) {
      const code = ms.class[id];
      if(typeof code != 'number') {
        continue;
      }
      result.push({
        code,
        id,
        class: ms.class,
        module: ms.module
      });
    }
  });
  return result;
}

const toTitleCase = (s: string) => s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();

function getMessageDetail(cls: any, id: string, escapeMarkdown: boolean): CompilerEvent {
  const o = /^(DEBUG|VERBOSE|INFO|HINT|WARN|ERROR|FATAL)_([A-Za-z0-9_]+)$/.exec(id);
  if(!o) {
    throw new Error(`Unexpected compiler message ${id}, does not match message error format`);
  }

  const f = toTitleCase(o[1]) + '_' + o[2];
  const event = cls[f]?.({} /* ignore arguments*/);
  if(!event) {
    throw new Error(`Call to ${cls.name}.${f} returned null`);
  }

  event.detail = dedentCompilerMessageDetail(event);
  event.message = event.message ?? '';
  event.message = event?.exceptionVar
    ? 'This is an internal error; the message will vary'
    : (escapeMarkdown ? `${escapeMarkdownChar(event.message, false)}` : event.message);

  return event;
}

/*---------------------------------------------------------------------------
 * Get messages in text format
 *---------------------------------------------------------------------------*/

function getMessagesAsText(messages: CompilerMessageDetail[]) {
  const result = messages.reduce((prev, message) => {
    return prev + '\n' + formatMessageAsText(getModuleName({class: message.class, module: message.module}), message.class, message.code, message.id);
  }, '');
  return result;
}

function formatMessageAsText(moduleName: string, cls: any, code: number, id: string) {
  const message = getMessageDetail(cls, id, false);
  return `${id}
* Code:      ${CompilerError.formatCode(code)}
* Module:    ${moduleName}
* Message:   ${message.message}
* Reference: ${helpUrl(code)}

${message.detail}
`;
}

/*---------------------------------------------------------------------------
 * Get messages as array of objects, for export to JSON
 *---------------------------------------------------------------------------*/

function getMessagesAsArrayForJson(messages: CompilerMessageDetail[]) {
  return messages.map(message => ({
    code: CompilerError.formatCode(message.code),
    id: message.id,
    class: message.class.name,
    module: message.module,
    detail: (() => getMessageDetail(message.class, message.id, false).detail)()
  }));
}

/*---------------------------------------------------------------------------
 * Export as Markdown
 *---------------------------------------------------------------------------*/

function messageCommandMarkdown(messages: string[], options: MessageOptions, callbacks: CompilerCallbacks) {
  if(messages.length) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessagesCannotBeFilteredForMarkdownFormat());
    return false;
  }
  if(!options.outPath) {
    callbacks.reportMessage(InfrastructureMessages.Error_OutputPathMustBeSpecifiedForMarkdownFormat());
    return false;
  }
  if(!fs.existsSync(options.outPath) || !fs.statSync(options.outPath)?.isDirectory()) {
    callbacks.reportMessage(InfrastructureMessages.Error_OutputPathMustExistAndBeADirectory({outPath:options.outPath}));
    return false;
  }
  exportAllMessagesAsMarkdown(options.outPath);
  return true;
}

function exportAllMessagesAsMarkdown(outPath: string) {
  let index =
`---
title: Compiler Messages Reference
---

`;

  messageNamespaceKeys.forEach((namespace: CompilerErrorNamespace) => {
    const ms = messageSources[namespace] as CompilerMessageSource;
    const moduleName = getModuleName(ms);
    index += `* [${moduleName}](${moduleName.toLowerCase()})\n`;
    exportModuleMessagesAsMarkdown(moduleName, ms, outPath);
  });

  fs.writeFileSync(path.join(outPath, 'index.md'), index, 'utf-8');
}

function exportModuleMessagesAsMarkdown(moduleName: string, ms: CompilerMessageSource, outPath: string) {
  const cls = ms.class;

  let index =
`---
title: Compiler Messages Reference for @keymanapp/${ms.module}
---

 Code | Identifier | Message
------|------------|---------
`;

  const ids = getMessageIdentifiersSorted(cls);
  for(const id of ids) {
    const code = cls[id];
    const filename = CompilerError.formatCode(code).toLowerCase();
    const message = getMessageDetail(cls, id, true);
    const content = formatMessageAsMarkdown(moduleName, id, message);
    index += `[${CompilerError.formatCode(code)}](${filename}) | \`${id}\` | ${message.message}\n`;
    fs.writeFileSync(path.join(outPath, filename + '.md'), content, 'utf-8');
  }

  fs.writeFileSync(path.join(outPath, moduleName.toLowerCase() + '.md'), index, 'utf-8');
}

function formatMessageAsMarkdown(moduleName: string, id: string, message: CompilerEvent) {
  return `---
title: ${CompilerError.formatCode(message.code)}: ${id}
---

|            |           |
|------------|---------- |
| Message    | ${message.message} |
| Module     | [${moduleName}](${moduleName.toLowerCase()}) |
| Identifier | \`${id}\` |

${message.detail}
`
}
