package org.hoshino9.luogu.contest

enum class RuleType {
	None,
	OI,
	ACM,
	LD,            // 乐多
	IOI,
	CF,
	DS            // 夺时
}

enum class VisibilityType {
	Banned,
	Official,
	OrganizationPublic,
	OrganizationInternal,
	PersonalPublic,
	PersonalInvite,
	OrganizationInvite,
	OrganizationPublicVerifying,
	PersonalPublicVerifying
}