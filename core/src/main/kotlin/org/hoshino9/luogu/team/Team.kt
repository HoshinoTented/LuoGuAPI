package org.hoshino9.luogu.team

interface IBaseTeam {
	val id: Int
	val name: String
}

data class BaseTeam(override val id: Int, override val name: String) : IBaseTeam