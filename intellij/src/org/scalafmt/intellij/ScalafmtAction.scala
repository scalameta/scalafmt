package org.scalafmt.intellij

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}

class ScalafmtAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit =
    Utils.getCurrentFileDocument(event).foreach(_.format())
}
