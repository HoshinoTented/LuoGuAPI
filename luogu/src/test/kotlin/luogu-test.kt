fun main() {
	LuoGuTest().run {
		login()
		println("logged in: $user")

		saveCookie()
		println("save cookie")
	}
}