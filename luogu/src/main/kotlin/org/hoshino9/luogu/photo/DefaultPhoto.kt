package org.hoshino9.luogu.photo

import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.utils.HasElement
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element

open class DefaultPhoto(override val elem : Element) : AbstractPhoto(), HasElement {
	private val leftElem : Element = run { elem.child(0) }
	private val rightElem : Element = run { elem.child(1) }

	override val date : String = run {
		leftElem.child(2).run {
			textNodes().lastOrNull { it.text().isNotBlank() }?.text()?.trim() ?: throw HTMLParseException(this)
		}
	}

	override val user : User = run {
		leftElem.child(2).child(0).attr("href").run(LuoGuUtils::userFromUrl)
	}

	override val url : String = run {
		rightElem.child(0).child(0).text()
	}
}