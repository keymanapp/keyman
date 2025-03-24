import chalk from 'chalk';
import { Token, Tokens } from 'marked';

const color = chalk.default;

export type MessageType = 'info' | 'warning' | 'error';

export const severityColors: {[value in MessageType]: chalk.Chalk} = {
  'info': color.reset,
  'warning': color.hex('FFA500'), // orange
  'error': color.redBright,
};

export interface LinkRefMessage {
  type: MessageType;
  message: string;
  token: Tokens.Link | Tokens.Image;
};

export interface LinkRef {file: string, links: Token[], messages: LinkRefMessage[]};
