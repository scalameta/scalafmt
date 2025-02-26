package org.scalafmt.sysops

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

private[sysops] object GranularDialectAsyncOps {

  object parasiticExecutionContext extends ExecutionContextExecutor {
    override final def execute(runnable: Runnable): Unit = runnable.run()
    override final def reportFailure(t: Throwable): Unit = ExecutionContext
      .defaultReporter(t)
  }

}
