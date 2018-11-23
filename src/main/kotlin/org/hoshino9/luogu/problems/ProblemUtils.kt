package org.hoshino9.luogu.problems

object ProblemUtils {
	fun getProblemIdFromUrl(url : String) : String {
		return url.substring(url.lastIndexOf('='))
	}
}