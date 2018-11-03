@file:Suppress("unused")

package org.hoshino9.luogu.problems.tags

import org.hoshino9.luogu.LuoGuTag

sealed class ProblemStore(text : String, data : Int) : LuoGuTag(text, data)

object LuoGu : ProblemStore("洛谷题库", 1)
object CodeForces : ProblemStore("CodeForces", 13)
object SPOJ : ProblemStore("SPOJ", 14)
object AtCoder : ProblemStore("AtCoder", 15)
object UVa : ProblemStore("UVa", 16)