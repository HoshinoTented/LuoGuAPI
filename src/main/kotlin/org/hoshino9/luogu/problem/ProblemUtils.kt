package org.hoshino9.luogu.problem

object ProblemUtils {
	fun getProblemIdFromUrl(url : String) : String {
		return url.substring(url.lastIndexOf('='))
	}
}