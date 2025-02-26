package org.scalafmt.sysops

import scala.concurrent.ExecutionContext

private[sysops] object GranularDialectAsyncOps {

  def parasiticExecutionContext: ExecutionContext.parasitic.type =
    ExecutionContext.parasitic

}
