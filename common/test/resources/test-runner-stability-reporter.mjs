import EventEmitter from 'node:events';

/**
 * @typedef LauncherError
 * @prop {string} sessionId
 * @prop {string} activeMethod
 * @prop {Error} error
 * @prop {string=} url
 */

/**
 * A wrapper object for the PlaywrightRunner type used with @web/test-runner to
 * launch tests in various browser engines.  This wrapper allows us to spy on
 * each function as it is called and detect when errors occur that are not otherwise
 * exposed by @web/test-runner.
 */
export class LauncherWrapper extends EventEmitter {
  /** @type {import("@web/test-runner-playwright").PlaywrightLauncher} */
  wrapped;

  /** @param launcher {import("@web/test-runner-playwright").PlaywrightLauncher} */
  constructor(launcher) {
    super();
    this.wrapped = launcher;
  }

  /**
   * @param config { import("@web/test-runner").TestRunnerCoreConfig }
   * @param testFiles { string[] }
   */
  async initialize(config, testFiles) {
    try {
      return await this.wrapped.initialize?.(config, testFiles);
    } catch(err) {
      try {
        /** @type {LauncherError} */
        const msg = {
          sessionId: '',
          activeMethod: 'initialize',
          error: err
        };
        this.emit('error', msg);
      } catch { /* Do not allow errors in our messaging to break the flow. */}
      throw err;
    }
  }

  async stop() {
    return this.wrapped.stop?.();
  }

  /**
   * @param sessionId {string}
   * @param url {string}
   */
  async startSession(sessionId, url) {
    try {
      return await this.wrapped.startSession(sessionId, url);
    } catch(err) {
      try {
        /** @type {LauncherError} */
        const msg = {
          sessionId: sessionId,
          activeMethod: 'startSession',
          error: err,
          url: url
        };
        this.emit('error', msg);
      } catch { /* Do not allow errors in our messaging to break the flow. */}
      throw err;
    }
  }

  /**
   * @param sessionId {string}
   */
  isActive(sessionId) {
    return this.wrapped.isActive(sessionId);
  }

  /**
   * @param sessionId {string}
   */
  getBrowserUrl(sessionId) {
    return this.wrapped.getBrowserUrl(sessionId);
  }

  /**
   * @param sessionId {string}
   * @param url {string}
   */
  async startDebugSession(sessionId, url) {
    return this.wrapped.startDebugSession(sessionId, url);
  }

  /** @param browser { import('playwright').Browser } */
  async createNewPage(browser) {
    try {
      return await this.wrapped.createNewPage(browser);
    } catch (err) {
      try {
        /** @type {LauncherError} */
        const msg = {
          sessionId: '',
          activeMethod: 'createNewPage',
          error: err
        };
        this.emit('error', msg);
      } catch { /* Do not allow errors in our messaging to break the flow. */}
      throw err;
    }
  }

  /** @param sessionId {string} */
  async stopSession(sessionId) {
    const lastUrl =  this.wrapped.getBrowserUrl(sessionId);
    try {
      return await this.wrapped.stopSession(sessionId);
    } catch (err) {
      try {
        /** @type {LauncherError} */
        const msg = {
          sessionId: sessionId,
          activeMethod: 'stopSession',
          error: err,
          url: lastUrl
        };
        this.emit('error', msg);
      } catch { /* Do not allow errors in our messaging to break the flow. */}
      throw err;
    }
  }

  /** @param sessionId {string} */
  getPage(sessionId) {
    try {
      return this.wrapped.getPage(sessionId);
    } catch(err) {
      try {
        /** @type {LauncherError} */
        const msg = {
          sessionId: sessionId,
          activeMethod: 'getPage',
          error: err
        };
        this.emit('error', msg);
      } catch { /* Do not allow errors in our messaging to break the flow. */}
      throw err;
    }
  }

  get name() {
    return this.wrapped.name;
  }

  set name(text) {
    this.wrapped.name = text;
  }

  get type() {
    return this.wrapped.type;
  }

  get concurrency() {
    return this.wrapped.concurrency;
  }
}

// Yay for references!
// https://www.jetbrains.com/help/teamcity/service-messages.html#Supported+Test+ServiceMessages
// https://modern-web.dev/docs/test-runner/reporters/write-your-own/

/**
 * Returns a custom reporter that allows us to report on testing stability
 * issues.  This will only provide useful information for launchers that are
 * wrapped with `LauncherWrapper`.
 */
export function sessionStabilityReporter({ ciMode = false } = {}) {
  // const name = "Test-session stability reporter";

  /** @type {import('@web/test-runner').Logger} */
  let logger;

  /** @type {Map<string, LauncherError[]} */
  let sessionMsgMap = new Map();

  function tcReportEscaping(data) {
    /** @type{string} */
    let text;

    if(typeof data == 'string') {
      text = data;
    } else if(data) {
      text = data.toString();
    } else {
      text = '';
    }

    return text
      .replace(/\|/g, '||')
      .replace(/\[/g, '|[')
      .replace(/]/g, '|]')
      .replace(/\'/g, '|\'');
  }

  const e = tcReportEscaping;

  /**
   * @param {import('@web/test-runner').BasicTestSession} session
   */
  const buildSessionName = (session) => `${session.group.name} - ${session.browser.name}`;

  /** @type {import('@web/test-runner').Reporter} */
  const reporter = {
    start({browsers, config, sessions}) {
      logger = config.logger;
      for(const browser of browsers) {
        if(browser instanceof EventEmitter) {
          browser.on('error', (msg) => {
            const list = sessionMsgMap.get(msg.sessionId) ?? [];
            if(!list.length) {
              sessionMsgMap.set(msg.sessionId, list);
            }
            list.push(msg);
          });
        }
      }
    },
    stop(args) {
      let errorReported = false;
      let failingTargets = [];

      for(const session of args.sessions) {
        const sessionId = session.id;
        const sessionName = buildSessionName(session);
        if(!failingTargets.find((entry) => entry == sessionName)) {
          failingTargets.push(sessionName);
        }
        const msgList = sessionMsgMap.get(sessionId) ?? [];
        if(msgList.length > 0) {
          errorReported = true;
          logger.log('');
          logger.log(`Error for session ${sessionName}`);
          logger.log(`  Affected test files: ${JSON.stringify(session.group.testFiles)}`);
          for(const msg of msgList) {
            if(msg.url) {
              logger.log(`  For URL: ${msg.url}`);
            }
            logger.log(`  In \`${msg.activeMethod}\`:`);
            logger.log(msg.error);
          }
        }
      }

      const globalMsgs = sessionMsgMap.get('') ?? [];
      if(globalMsgs.length > 0) {
        errorReported = true;
        failingTargets.unshift('General setup');
        logger.log("General errors:")
        for(const msg of globalMsgs) {
          logger.log('');
          logger.log(`  In \`${msg.activeMethod}\`:`);
          logger.log(msg.error);
        }
      }

      if(errorReported) {
        logger.log('');
        if(ciMode) {
          logger.log(`##teamcity[message text='${e('One or more stability errors occurred during testing for the following test groups')}' errorDetails='${JSON.stringify(failingTargets)}' status='ERROR']`);
        } else {
          logger.log(`Stability summary: failures occurred for the following test groups: ${JSON.stringify(failingTargets)}`);
        }
      }
    }
  };

  return reporter;
}