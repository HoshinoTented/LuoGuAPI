package org.hoshino9.luogu.page

import cats.effect.IO
import com.google.gson.{JsonObject, JsonParser}
import org.hoshino9.luogu.LuoGuClient

trait LuoGuPage {
  def currentData: JsonObject

  def refresh(): Unit
}

trait LuoGuClientPage extends LuoGuPage {
  protected var _currentData: Option[JsonObject] = None

  def url: String

  def client: LuoGuClient

  def content(): String = {
    client.get(url).body().string()
  }

  override def currentData: JsonObject = {
    _currentData match {
      case Some(data) => data
      case None => throw new NullPointerException("currentData == None")
    }
  }

  override def refresh(): Unit = {
    val content = this.content()
    _currentData = Some(JsonParser.parseString(content).getAsJsonObject.getAsJsonObject("currentData"))
  }
}