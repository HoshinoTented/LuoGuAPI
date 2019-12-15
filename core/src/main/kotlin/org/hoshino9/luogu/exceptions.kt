@file:Suppress("unused", "CanBeParameter", "MemberVisibilityCanBePrivate", "FunctionName")

package org.hoshino9.luogu

import okhttp3.Response
import org.jsoup.nodes.Node

open class IllegalStatusCodeException(val code : Any?, msg : Any? = null) : IllegalStateException("$code: $msg")
open class HTMLParseException(val node : Node?, val msg : String = "") : Exception(msg)