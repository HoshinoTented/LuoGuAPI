package org.hoshino9.luogu

import java.lang.reflect.Type

import com.google.gson.{JsonDeserializationContext, JsonDeserializer, JsonElement}

import scala.reflect.ClassTag

package object json {

	abstract class Redirect[From, To <: From](implicit val tag: ClassTag[To]) extends JsonDeserializer[From] {
		override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): From = {
			context.deserialize(json, tag.runtimeClass)
		}
	}

}
