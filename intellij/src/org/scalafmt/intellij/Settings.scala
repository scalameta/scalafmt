package org.scalafmt.intellij

import scala.beans.BeanProperty

import com.intellij.openapi.components._
import com.intellij.util.xmlb.XmlSerializerUtil

@State(name = "ScalafmtSettings",
       storages = Array(new Storage("ScalafmtSettings.xml")))
class Settings extends PersistentStateComponent[Settings] {
  @BeanProperty
  var formatOnSave: Boolean = false

  override def loadState(config: Settings): Unit =
    XmlSerializerUtil.copyBean(config, this)

  override def getState: Settings = this
}

object Settings {
  def apply() = ServiceManager.getService(classOf[Settings])
}
