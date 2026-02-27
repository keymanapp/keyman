/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import type {
  FullConfig, FullResult, Reporter, Suite, TestCase, TestResult
} from '@playwright/test/reporter';


class TestNode {
  public static Nodes = new Map<string, TestNode>();
  private static OpenNodes: string[] = [];
  public static RootFlow: string;
  private id: string;
  private suiteOrTest: Suite | TestCase;
  private flowId: number;
  private parent: TestNode;
  private childrenToVisit: TestNode[] = [];

  public constructor(suiteOrTest: Suite | TestCase) {
    this.suiteOrTest = suiteOrTest;
    this.id = this.suiteOrTest.titlePath().toString();
    this.flowId = Math.floor(Math.random() * 100000 + 1);
    this.parent = null; // will be set by parent
    if (!this.isTest) {
      for (const child of (<Suite>suiteOrTest).entries()) {
        const node = new TestNode(child);
        this.childrenToVisit.push(node);
        node.parent = this;
      }
    }
    TestNode.Nodes.set(this.id, this);
  }

  private get isTest(): boolean {
    return this.suiteOrTest.type == 'test';
  }

  public get parentFlowId(): number | string {
    return this.parent?.flowId ?? TestNode.RootFlow;
  }

  private start(): void {
    this.parent?.start();
    if (TestNode.OpenNodes.includes(this.id)) {
      // already started, nothing to do
      if (this.isTest) {
        console.error(`Test '${this.suiteOrTest.title}' is already started!`)
      }
      return;
    }

    if (this.suiteOrTest.title !== '') {
      console.log(`##teamcity[flowStarted flowId='${this.flowId}' parent='${this.parentFlowId}']`);
      if (this.isTest) {
        console.log(`##teamcity[testStarted name='${this.suiteOrTest.title}' captureStandardOutput='true']`);
      } else {
        console.log(`##teamcity[testSuiteStarted name='${this.suiteOrTest.title}']`);
      }
    }

    TestNode.OpenNodes.push(this.id);
  }

  private escape(message: string): string {
    // TeamCity escaping rules for: ' | [ ] \n \r \uNNNN
    // See: https://www.jetbrains.com/help/teamcity/service-messages.html#Escaped+Values
    return message?.replace(/['|\[\]]/g, (matched) => `|${matched}`)
      .replace(/\n/g, '|n')
      .replace(/\r/g, '|r')
      .replace(/[\u0080-\uFFFF]/g, c => `|0x${c.charCodeAt(0).toString(16).padStart(4, '0')}`) ?? '';
  }

  private getTestResult(result: TestResult): { msgTitle: string, details: string } {
    if (!result) {
      return null;
    }
    switch (result.status) {
      case 'passed':
        return { msgTitle: 'testFinished', details: `duration='${result.duration}'` };
      case 'failed':
      case 'interrupted':
      case 'timedOut':
        return { msgTitle: 'testFailed', details: `message='${this.escape(result.error?.message)}' details='${this.escape(result.error?.value ?? result.error?.cause)}'` };
      case 'skipped':
        return { msgTitle: 'testIgnored', details: `message='${this.escape(result.annotations?.toString()) ?? ''}'` };
    }
  }

  private end(result: TestResult, force: boolean = false): void {
    if (this.childrenToVisit.length > 0 && force) {
      while (this.childrenToVisit.length > 0) {
        const child = this.childrenToVisit[0];
        child.end(result, force);
      }
    }

    if (!force) {
      if (this.suiteOrTest.title !== '') {
        if (this.isTest) {
          const { msgTitle, details } = this.getTestResult(result) ?? { msgTitle: 'testFinished', details: '' };
          console.log(`##teamcity[${msgTitle} name='${this.suiteOrTest.title}' ${details}]`);
        } else {
          console.log(`##teamcity[testSuiteFinished name='${this.suiteOrTest.title}']`);
        }
        console.log(`##teamcity[flowFinished flowId = '${this.flowId}']`);
      }
      this.removeFromOpenNodes();

      this.parent?.removeChild(this);
      TestNode.Nodes.delete(this.id);
    }
  }

  private removeFromOpenNodes(): void {
    const ourIndex = TestNode.OpenNodes.indexOf(this.id);
    if (ourIndex < 0) {
      console.error(`Can't find '${this.id}' in open nodes`);
      return;
    }
    TestNode.OpenNodes.splice(ourIndex, 1);
  }

  private removeChild(child: TestNode) {
    const childIndex = this.childrenToVisit.indexOf(child);
    if (childIndex < 0) {
      console.error(`Can't find child '${child.id}' in parent '${this.id}'`);
      return;
    }
    this.childrenToVisit.splice(childIndex, 1);

    if (this.childrenToVisit.length > 0) {
      return;
    }

    // No more children, so close this node
    this.end(null, false);
  }

  public static startTest(test: TestCase): void {
    const node = TestNode.Nodes.get(test.titlePath().toString());
    if (!node) {
      console.error(`Can't find test node for ${test.titlePath().toString()}`);
      return;
    }
    node.start();
  }

  public static endTest(test: TestCase, result: TestResult): void {
    const node = TestNode.Nodes.get(test.titlePath().toString());
    if (!node) {
      console.error(`Can't find test node for ${test.titlePath().toString()}`);
      return;
    }
    node.end(result);
  }

  public endAll(): void {
    if (this.childrenToVisit.length > 0) {
      console.error(`Root node still has ${this.childrenToVisit.length} open children`);
    }
    this.end(null, true);
    if (TestNode.OpenNodes.length > 0) {
      console.error(`Still have ${TestNode.OpenNodes.length} open nodes`);
      while (TestNode.OpenNodes.length > 0) {
        const id = TestNode.OpenNodes[TestNode.OpenNodes.length - 1];
        console.log(`Closing '${id}'`);
        const node = TestNode.Nodes.get(id);
        node.end(null);
      }
    }
    if (TestNode.Nodes.size > 0) {
      console.error(`Still have ${TestNode.Nodes.size} nodes hanging around`);
      for (const [id, node] of Array.from(TestNode.Nodes.entries())) {
        console.error(`Remaining node title: '${node.suiteOrTest.title}'`);
      }
    }
  }
}

export default class PlaywrightTeamcityReporter implements Reporter {
  private root: TestNode = null;

  public constructor(options: { parentFlow?: string } = {}) {
    TestNode.RootFlow = options.parentFlow ?? 'unit_tests';
  }

  public onBegin(config: FullConfig, suite: Suite) {
    this.root = new TestNode(suite);
  }

  public onTestBegin(test: TestCase, result: TestResult) {
    TestNode.startTest(test);
  }

  public onTestEnd(test: TestCase, result: TestResult) {
    TestNode.endTest(test, result);
  }

  public onEnd(result: FullResult) {
    this.root.endAll();
  }
}
