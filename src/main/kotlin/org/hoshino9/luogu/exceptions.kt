@file:Suppress("unused", "CanBeParameter", "MemberVisibilityCanBePrivate")

package org.hoshino9.luogu

open class LuoGuException(val luoGu : LuoGu, msg : String) : Exception(msg)
open class LuoGuUserException(val user : LuoGuUser, msg : String) : LuoGuException(user.luogu, msg)