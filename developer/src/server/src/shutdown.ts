import { KeymanSentry } from '@keymanapp/developer-utils';

export async function shutdown(code?: number): Promise<never> {
  await KeymanSentry.close();
  process.exit(code ?? 0);
}
