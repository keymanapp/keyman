import chalk from 'chalk';
import { Token, Tokens } from 'marked';

export type MessageType = 'info' | 'warning' | 'error';

export const severityColors: {[value in MessageType]: chalk.Chalk} = {
  'info': chalk.reset,
  'warning': chalk.hex('FFA500'), // orange
  'error': chalk.redBright,
};

export interface LinkRefMessage {
  type: MessageType;
  message: string;
  token: Tokens.Link | Tokens.Image;
};

export interface LinkRef {file: string, links: Token[], messages: LinkRefMessage[]};
