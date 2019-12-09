package org.hoshino9.luogu.contest

import org.hoshino9.luogu.LuoGu

fun LuoGu.contestListPage(page: Int = 1): ContestListPage {
	return ContestListPage(page, client)
}