@file:Suppress("unused", "CanBeParameter", "MemberVisibilityCanBePrivate")

package org.hoshino9.luogu

open class StatusException(msg : String) : Exception(msg)
open class StatusCodeException(val code : Int, msg : String = "") : StatusException("$code: $msg")
open class LuoGuException(val luoGu : LuoGu, msg : String) : Exception(msg)
open class LuoGuStatusCodeException(luoGu : LuoGu, val code : Int, msg : String) : LuoGuException(luoGu, msg)
open class LuoGuUserException(val user : LuoGuLoggedUser, msg : String) : LuoGuException(user.luogu, msg)