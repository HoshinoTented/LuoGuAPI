package org.hoshino9.luogu.page

trait MutableLuoGuPage extends LuoGuPage with MutableLuoGuPageOps {
	override def refresh(): Unit = super.refresh()
}
