@file:Suppress("unused")

package org.hoshino9.luogu.problems.tags

import org.hoshino9.luogu.LuoGuTag

abstract class ProblemStore(text : String, data : Int) : LuoGuTag(text, data)

sealed class LuoGu : ProblemStore("洛谷题库", 1)
sealed class CodeForces : ProblemStore("CodeForces", 13)
sealed class SPOJ : ProblemStore("SPOJ", 14)
sealed class AtCoder : ProblemStore("AtCoder", 15)
sealed class UVa : ProblemStore("UVa", 16)