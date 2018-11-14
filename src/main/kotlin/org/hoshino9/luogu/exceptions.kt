@file:Suppress("unused", "CanBeParameter", "MemberVisibilityCanBePrivate", "FunctionName")

package org.hoshino9.luogu

import okhttp3.Response
import org.jsoup.nodes.Node

open class StatusException(msg : String) : Exception(msg)
open class StatusCodeException(val code : Int, msg : String = "") : StatusException("$code: $msg")
open class APIStatusCodeException(code : Int, msg : String = "") : StatusCodeException(code, msg)        //主要用于分辨 请求错误 和 接口错误
open class LuoGuException(val luoGu : LuoGu, msg : String) : Exception(msg)
open class LuoGuStatusCodeException(luoGu : LuoGu, val code : Int, msg : String) : LuoGuException(luoGu, msg)
open class LuoGuUserException(val user : LuoGuLoggedUser, msg : String) : LuoGuException(user.luogu, msg)
open class MatchException(val regex : Regex, val seq : CharSequence) : Exception(""""$seq"cannot match "$regex"""")
open class HTMLParseException(val node : Node, val msg : String = "") : Exception(msg)

fun StatusCodeException(resp : Response) : StatusCodeException = StatusCodeException(resp.code())