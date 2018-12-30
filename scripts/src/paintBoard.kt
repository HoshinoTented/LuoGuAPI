import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import java.awt.Color
import java.io.OutputStream
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

val colorList = arrayOf(
		Color(0, 0, 0),
		Color(255, 255, 255),
		Color(170, 170, 170),
		Color(85, 85, 85),
		Color(254, 211, 199),
		Color(255, 196, 206),
		Color(250, 172, 142),
		Color(255, 139, 131),
		Color(244, 67, 54),
		Color(233, 30, 99),
		Color(226, 102, 158),
		Color(156, 39, 176),
		Color(103, 58, 183),
		Color(63, 81, 181),
		Color(0, 70, 112),
		Color(5, 113, 151),
		Color(33, 150, 243),
		Color(0, 188, 212),
		Color(59, 229, 219),
		Color(151, 253, 220),
		Color(22, 115, 0),
		Color(55, 169, 60),
		Color(137, 230, 66),
		Color(215, 255, 7),
		Color(255, 246, 209),
		Color(248, 203, 140),
		Color(255, 235, 59),
		Color(255, 193, 7),
		Color(255, 152, 0),
		Color(255, 87, 34),
		Color(184, 63, 39),
		Color(121, 85, 72)
)

/**
 * 画板绘画函数
 *
 * @param x 格子的横坐标(左上为0)
 * @param y 格子的纵坐标(左上为0)
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性 或 使用先进的IDE在上方的 colorList 预览颜色)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) {
	executePost("paintBoard/paint",
			mapOf(
					"x" to x.toString(),
					"y" to y.toString(),
					"color" to color.toString()
			).params(), referer("paintBoard")) { resp ->
		resp.assert()
		println(resp.data)
	}
}

/**
 * 获取画板图片
 *
 * @param out 输出的流
 */
fun LuoGu.board(out : OutputStream) {
	executeGet("paintBoard/board") { resp ->
		resp.assert()

		resp.data?.let { board ->
			BufferedImage(800, 400, BufferedImage.TYPE_INT_RGB).also { image ->
				board.split('\n').forEachIndexed { x, line ->
					line.forEachIndexed { y, char ->
						image.setRGB(x, y, colorList[char.toString().toInt(32)].rgb)
					}
				}
			}.let { image ->
				ImageIO.write(image, "PNG", out)
			}
		}
	}
}