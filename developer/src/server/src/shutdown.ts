import { KeymanSentry } from './KeymanSentry.js';

export async function shutdown(code?: number): Promise<never> {
  await KeymanSentry.close();
  process.exit(code ?? 0);
}
