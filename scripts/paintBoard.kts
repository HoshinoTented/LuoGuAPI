import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*

/**
 * # 画板绘画函数  
 * 如果成功, 则会打印出一串 `Json`  
 * 类似: `{data:[], status:200}`  
 * 如果失败, 则会打印`failed`  
 *
 * @param x 格子的横坐标(左上为0)
 * @param y 格子的纵坐标(左上为0)
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) {
	postExecute("paintBoard/paint", mapOf("x" to x.toString(), "y" to y.toString(), "color" to color.toString()).params(), referer("paintBoard")) { resp ->
		resp.assert()
		json (resp.data!!) {
			if (getInt("status") == 200) {
				println(this)
			} else {
				println(("failed"))
			}
		}
	}
}
