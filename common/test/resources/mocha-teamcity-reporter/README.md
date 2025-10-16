# Custom mocha reporter for TeamCity

This is an enhanced version of the [mocha-teamcity-reporter](https://github.com/travisjeffery/mocha-teamcity-reporter)
(renamed to *.cjs) that allows to specify the parent flowId so that the tests
can be grouped together under the parent block in TeamCity.

If [travisjeffery/mocha-teamcity-reporter#69](https://github.com/travisjeffery/mocha-teamcity-reporter/issues/69)
gets fixed one day, we can go back to using the original version.
