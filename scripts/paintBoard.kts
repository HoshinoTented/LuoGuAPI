import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*

/*
操作实例(Kotlin REPL, 已登录, 客户端对象名为 luogu):

luogu.draw(0, 0, 0)			//在 (0, 0) 的格子上画上黑色
 */

/**
 * 画板绘画函数
 *
 * @param x 格子的横坐标(左上为0)
 * @param y 格子的纵坐标(左上为0)
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) {
	postExecute("paintBoard/paint", mapOf("x" to x.toString(), "y" to y.toString(), "color" to color.toString()).params(), referer("paintBoard")) { resp ->
		resp.assert()
		json (resp.data!!) {
			println(this)
		}
	}
}
