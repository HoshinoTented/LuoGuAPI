@file:Suppress("unused", "CanBeParameter", "MemberVisibilityCanBePrivate", "FunctionName")

package org.hoshino9.luogu

import okhttp3.Response
import org.jsoup.nodes.Node

open class IllegalStatusCodeException(val code : Int, msg : String = "") : IllegalStateException("$code: $msg")
open class IllegalAPIStatusCodeException(code : Int, msg : String = "") : IllegalStatusCodeException(code, msg)        //主要用于分辨 请求错误 和 接口错误
open class MatchException(val regex : Regex, val seq : CharSequence) : Exception(""""$seq"cannot match "$regex"""")
open class HTMLParseException(val node : Node, val msg : String = "") : Exception(msg)

fun StatusCodeException(resp : Response) : IllegalStatusCodeException = IllegalStatusCodeException(resp.code())