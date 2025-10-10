/* eslint-disable complexity */
/**
 * Teamcity doc reference https://confluence.jetbrains.com/display/TCD10/Build+Script+Interaction+with+TeamCity
 *
 * Module dependencies.
 */
'use strict';

const processPID = process.pid.toString();
const TEST_IGNORED = `##teamcity[testIgnored name='%s' message='%s']`;
const SUITE_START = `##teamcity[testSuiteStarted name='%s']`;
const SUITE_END = `##teamcity[testSuiteFinished name='%s' duration='%s']`;
const SUITE_END_NO_DURATION = `##teamcity[testSuiteFinished name='%s']`;
const TEST_START = `##teamcity[testStarted name='%s' captureStandardOutput='true']`;
const TEST_FAILED = `##teamcity[testFailed name='%s' message='%s' details='%s' captureStandardOutput='true']`;
const TEST_FAILED_COMPARISON = `##teamcity[testFailed type='comparisonFailure' name='%s' message='%s' \
details='%s' captureStandardOutput='true' actual='%s' expected='%s']`;
const TEST_END = `##teamcity[testFinished name='%s' duration='%s']`;
const TEST_END_NO_DURATION = `##teamcity[testFinished name='%s']`;
const FLOW_START = `##teamcity[flowStarted flowId='%s' parent='%s']`;
const FLOW_END = `##teamcity[flowFinished flowId='%s']`;

const Mocha = require('mocha');
const {
	EVENT_SUITE_BEGIN,
	EVENT_TEST_BEGIN,
	EVENT_TEST_FAIL,
	EVENT_TEST_PENDING,
	EVENT_TEST_END,
	EVENT_HOOK_BEGIN,
	EVENT_HOOK_END,
	EVENT_SUITE_END,
	EVENT_RUN_END
} = Mocha.Runner.constants;

const util = require('util');

let Base, log, logError;

Base = require('mocha').reporters.Base;
log = console.log;
logError = console.error;

let flowIds = [];

/**
 * Escape the given `str`.
 */

function escape(str) {
	if (!str) return '';
	return str
		.toString()
		.replace(/\x1B.*?m/g, '') // eslint-disable-line no-control-regex
		.replace(/\|/g, '||')
		.replace(/\n/g, '|n')
		.replace(/\r/g, '|r')
		.replace(/\[/g, '|[')
		.replace(/\]/g, '|]')
		.replace(/\u0085/g, '|x')
		.replace(/\u2028/g, '|l')
		.replace(/\u2029/g, '|p')
		.replace(/'/g, '|\'');
}

function isNil(value) {
	return value == null; 	// eslint-disable-line
}

function formatString() {
	let formattedArguments = [];
	const args = Array.prototype.slice.call(arguments, 0);
	// Format all arguments for TC display (it escapes using the pipe char).
	let tcMessage = args.shift();
	args.forEach((param) => {
		formattedArguments.push(escape(param));
	});
	formattedArguments.unshift(tcMessage);
	return util.format.apply(util, formattedArguments);
}

function handleFlow(isStarted, nestFlows) {
	if (!nestFlows) {
		return;
	}
	if (isStarted) {
		flowIds.push(Math.floor(Math.random() * 100000 + 1));
		log(formatString(FLOW_START, flowIds[flowIds.length - 1], flowIds[flowIds.length - 2]));
	} else {
		log(formatString(FLOW_END, flowIds[flowIds.length - 1]));
		flowIds.pop();
	}
}

function getFlowId() {
	return flowIds[flowIds.length - 1];
}

/**
 * Initialize a new `Teamcity` reporter.
 *
 * @param {Runner} runner
 * @param {options} options
 * @api public
 */

function Teamcity(runner, options) {
	options = options || {};
	const reporterOptions = options.reporterOptions || {};
	let flowIdOpt, useStdError, recordHookFailures, actualVsExpected, ignoreHookWithName, displayIgnoredAsIgnored;
	(reporterOptions.flowId) ? flowIdOpt = reporterOptions.flowId : flowIdOpt = process.env['MOCHA_TEAMCITY_FLOWID'] || processPID;
	(reporterOptions.useStdError) ? useStdError = reporterOptions.useStdError : useStdError = process.env['USE_STD_ERROR'];
	(reporterOptions.recordHookFailures) ? recordHookFailures = reporterOptions.recordHookFailures : recordHookFailures = process.env['RECORD_HOOK_FAILURES'];
	(reporterOptions.actualVsExpected) ? actualVsExpected = reporterOptions.actualVsExpected : actualVsExpected = process.env['ACTUAL_VS_EXPECTED'];
	(reporterOptions.ignoreHookWithName) ? ignoreHookWithName = reporterOptions.ignoreHookWithName : ignoreHookWithName = process.env['IGNORE_HOOK_WITH_NAME'];
	(reporterOptions.displayIgnoredAsIgnored)
		? displayIgnoredAsIgnored = reporterOptions.displayIgnoredAsIgnored
		: displayIgnoredAsIgnored = process.env['DISPLAY_IGNORED_AS_IGNORED'];
	(useStdError) ? useStdError = (useStdError.toLowerCase() === 'true') : useStdError = false;
	(recordHookFailures) ? recordHookFailures = (recordHookFailures.toLowerCase() === 'true') : recordHookFailures = false;
	(displayIgnoredAsIgnored) ? displayIgnoredAsIgnored = (displayIgnoredAsIgnored.toLowerCase() === 'true') : displayIgnoredAsIgnored = false;
	(ignoreHookWithName) ? ignoreHookWithName : null;
	actualVsExpected ? actualVsExpected = (actualVsExpected.toLowerCase() === 'true') : actualVsExpected = false;
	Base.call(this, runner);
	let stats = this.stats;
	const topLevelSuite = reporterOptions.topLevelSuite || process.env['MOCHA_TEAMCITY_TOP_LEVEL_SUITE'];
	const parentFlowId = reporterOptions.parentFlowId || process.env['MOCHA_TEAMCITY_PARENT_FLOW_ID'];
	if (parentFlowId) {
		flowIds.push(parentFlowId);
	} else {
		flowIds.push(flowIdOpt);
	}
	const hasParentFlowId = !!parentFlowId;

	const ignoredTests = {};
	const testState = { pending: 0 };

	runner.on(EVENT_SUITE_BEGIN, function (suite) {
		handleFlow(true, hasParentFlowId);
		if (suite.root) {
			if (topLevelSuite) {
				log(formatString(SUITE_START, topLevelSuite));
			}
			return;
		}
		suite.startDate = new Date();
		log(formatString(SUITE_START, suite.title));
	});

	runner.on(EVENT_TEST_BEGIN, function (test) {
		if (displayIgnoredAsIgnored && ignoredTests[`${test.title}-${getFlowId()}`] === testState.pending) {
			return;
		}
		handleFlow(true, hasParentFlowId);
		log(formatString(TEST_START, test.title));
	});

	runner.on(EVENT_TEST_FAIL, function (test, err) {
		let isHook = false;
		if (test.title.includes(`"before all" hook`) ||
			test.title.includes(`"before each" hook`) ||
			test.title.includes(`"after all" hook`) ||
			test.title.includes(`"after each" hook`)
		) {
			isHook = true;
		}

		if(actualVsExpected && (err.actual && err.expected)){
			if (useStdError) {
				logError(formatString(TEST_FAILED_COMPARISON,
					test.title, err.message, err.stack, err.actual, err.expected));
			} else {
				log(formatString(TEST_FAILED_COMPARISON, test.title, err.message, err.stack, err.actual, err.expected));
			}
		} else{
			if (useStdError) {
				logError(formatString(TEST_FAILED, test.title, err.message, err.stack));
			} else {
				log(formatString(TEST_FAILED, test.title, err.message, err.stack));
			}
		}
		// Log testFinished for failed hook (hook end event is not fired for failed hook)
		if (recordHookFailures && !ignoreHookWithName || recordHookFailures && ignoreHookWithName && !test.title.includes(ignoreHookWithName)) {
			if (isHook) {
				if(isNil(test.duration)){
					log(formatString(TEST_END_NO_DURATION, test.title));
				} else {
					log(formatString(TEST_END, test.title, test.duration.toString()));
				}
				handleFlow(false, hasParentFlowId);
			}
		}
	});

	runner.on(EVENT_TEST_PENDING, function (test) {
		log(formatString(TEST_IGNORED, test.title, test.title));
		if (displayIgnoredAsIgnored) {
			ignoredTests[`${test.title}-${getFlowId()}`] = testState.pending;
		} else {
			handleFlow(true, hasParentFlowId)
		}
	});

	runner.on(EVENT_TEST_END, function (test) {
		if (displayIgnoredAsIgnored && ignoredTests[`${test.title}-${getFlowId()}`] === testState.pending) {
			delete ignoredTests[`${test.title}-${getFlowId()}`];
			return;
		}
		if(isNil(test.duration)){
			log(formatString(TEST_END_NO_DURATION, test.title));
		} else {
			log(formatString(TEST_END, test.title, test.duration.toString()));
		}
		handleFlow(false, hasParentFlowId);
	});

	runner.on(EVENT_HOOK_BEGIN, function (test) {
		if (recordHookFailures && !ignoreHookWithName || recordHookFailures && ignoreHookWithName && !test.title.includes(ignoreHookWithName)) {
			handleFlow(true, hasParentFlowId);
			log(formatString(TEST_START, test.title));
		}
	});

	runner.on(EVENT_HOOK_END, function (test) {
		if (recordHookFailures && !ignoreHookWithName || recordHookFailures && ignoreHookWithName && !test.title.includes(ignoreHookWithName)) {
			if(isNil(test.duration)){
				log(formatString(TEST_END_NO_DURATION, test.title));
			} else {
				log(formatString(TEST_END, test.title, test.duration.toString()));
			}
			handleFlow(false, hasParentFlowId);
		}
	});

	runner.on(EVENT_SUITE_END, function (suite) {
		if (!suite.root) {
			log(formatString(SUITE_END, suite.title, new Date() - suite.startDate));
		}
		handleFlow(false, hasParentFlowId);
	});

	runner.on(EVENT_RUN_END, function () {
		let duration;
		(typeof stats === 'undefined') ? duration = null : duration = stats.duration;
		if (topLevelSuite) {
			isNil(duration)
				? log(formatString(SUITE_END_NO_DURATION, topLevelSuite))
				: log(formatString(SUITE_END, topLevelSuite, duration));
			handleFlow(false, hasParentFlowId);
		}
	});
}


/**
 * Expose `Teamcity`.
 */

exports = module.exports = Teamcity;
