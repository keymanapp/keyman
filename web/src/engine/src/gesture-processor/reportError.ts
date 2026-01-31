export function reportError(baseMsg: string, err: Error) {
  // Our mobile-app Sentry logging will listen for this and log it.
  if(err instanceof Error) {
    console.error(`${baseMsg}: ${err.message}\n\n${err.stack}`);
  } else {
    console.error(baseMsg);
    console.error(err);
  }
}