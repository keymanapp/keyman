import { KeymanSentry } from "@keymanapp/developer-utils";

/**
 * Exit codes defined in <sysexits.h>:
 * https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
 */
export const enum SysExits {
  EX_USAGE = 64,
  EX_DATAERR = 65,
};

export async function exitProcess(exitCode?: number): Promise<never> {
  await KeymanSentry.close();
  process.exit(exitCode);
}