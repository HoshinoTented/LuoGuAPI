@file:Suppress("unused")

package org.hoshino9.luogu.photo

import io.ktor.http.ContentType
import io.ktor.http.content.OutgoingContent
import kotlinx.coroutines.io.ByteWriteChannel
import kotlinx.coroutines.io.writeStringUtf8

sealed class Part {
	abstract val name: String
	abstract val body: () -> ByteArray

	data class Pair(override val name: String, val value: String) : Part() {
		override val body: () -> ByteArray
			get() = { value.toByteArray() }
	}

	data class File(override val name: String, val file: java.io.File, val contentType: ContentType) : Part() {
		override val body: () -> ByteArray
			get() = { file.readBytes() }
	}
}

class PhotoContent(val parts: List<Part>) : OutgoingContent.WriteChannelContent() {
	private val boundary: String = "hoshinosekaiichibankawaii"
	private val separator: String = "\r\n--$boundary"

	override val contentType: ContentType?
		get() = ContentType.MultiPart.FormData.withParameter("boundary", boundary)

	override suspend fun writeTo(channel: ByteWriteChannel) {
		parts.forEachIndexed { i, it ->
			channel.writeStringUtf8(if (i == 0) separator.drop(2) else separator)
			channel.writeStringUtf8("\r\n")
			channel.writeStringUtf8("Content-Disposition: form-data; name=\"${it.name}\"")

			when (it) {
				is Part.Pair -> {
					channel.writeStringUtf8("\r\n\r\n")
					it.body().forEach {
						channel.writeByte(it)
					}
				}

				is Part.File -> {
					channel.writeStringUtf8("; filename=\"${it.file.name}\"\r\n")
					channel.writeStringUtf8("Content-Type: ${it.contentType.contentType}/${it.contentType.contentSubtype}\r\n")
					channel.writeStringUtf8("Content-Length: ${it.file.length()}\r\n")
					channel.writeStringUtf8("\r\n")

					it.body().forEach {
						channel.writeByte(it)
					}
				}
			}

		}

		channel.writeStringUtf8(separator)
		channel.writeStringUtf8("--")
	}
}