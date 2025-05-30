import sys
from teamcity.messages import TeamcityServiceMessages
from teamcity.unittestpy import TeamcityTestResult, TeamcityTestRunner
from unittest import main


class KeymanTeamcityServiceMessages(TeamcityServiceMessages):
    def __init__(self, parent_flow='unit_tests'):
        self.parent_flow = parent_flow
        super().__init__()

    def testStarted(self, testName, captureStandardOutput=None, flowId=None, metainfo=None):
        self.message('testStarted', name=testName, metainfo=metainfo)
        self.message('flowStarted', flowId=flowId, parent=self.parent_flow)

    def testFinished(self, testName, testDuration=None, flowId=None):
        self.message('flowFinished', flowId=flowId, parent=self.parent_flow)
        if testDuration is not None:
            duration_ms = testDuration.days * 86400000 + \
                testDuration.seconds * 1000 + \
                int(testDuration.microseconds / 1000)
            self.message('testFinished', name=testName, duration=str(duration_ms))
        else:
            self.message('testFinished', name=testName)

    def testIgnored(self, testName, message='', flowId=None):
        self.message('flowFinished', flowId=flowId, parent=self.parent_flow)
        self.message('testIgnored', name=testName, message=message)

    def testFailed(self, testName, message='', details='', flowId=None, comparison_failure=None):
        self.message('flowFinished', flowId=flowId, parent=self.parent_flow)
        if not comparison_failure:
            self.message('testFailed', name=testName, message=message, details=details)
        else:
            diff_message = u"\n{0} != {1}\n".format(comparison_failure.actual, comparison_failure.expected)
            self.message(
                'testFailed',
                name=testName,
                message=str(message) + diff_message,
                details=details,
                type="comparisonFailure",
                actual=comparison_failure.actual,
                expected=comparison_failure.expected,
            )


class KeymanTestResult(TeamcityTestResult):
    def __init__(self, stream=sys.stdout, descriptions=None, verbosity=None):
        super().__init__(stream, descriptions, verbosity)
        self.messages = KeymanTeamcityServiceMessages()


class KeymanTestRunner(TeamcityTestRunner):
    resultclass = KeymanTestResult

    def run(self, test):
        return super().run(test)


if __name__ == '__main__':
    main(module=None, testRunner=KeymanTestRunner())
