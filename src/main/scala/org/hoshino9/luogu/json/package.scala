package org.hoshino9.luogu

import java.lang.reflect.Type

import com.google.gson.reflect.TypeToken
import com.google.gson.stream.{JsonReader, JsonWriter}
import com.google.gson.{Gson, GsonBuilder, JsonDeserializationContext, JsonDeserializer, JsonElement, TypeAdapter, TypeAdapterFactory}

import scala.reflect.ClassTag

package object json {
	type JavaList[T] = java.util.List[T]

	val global: Gson = new GsonBuilder()
					.create()

	abstract class Redirect[From, To <: From](implicit val tag: ClassTag[To]) extends JsonDeserializer[From] {
		override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): From = {
			context.deserialize(json, tag.runtimeClass)
		}
	}

}
