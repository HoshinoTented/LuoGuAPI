import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.extensions.paintboard.PaintUser
import org.hoshino9.luogu.extensions.paintboard.drawFromImage
import java.nio.file.Paths
import java.util.Properties
import javax.imageio.ImageIO

object PaintBoard {
	private val resourcesPath = Paths.get("src/test/resources")
	private val cookiesPath = resourcesPath.resolve("cookies.properties")
	private val imagePath = resourcesPath.resolve("cirno.png")

	private fun loadFromCookies() : List<PaintUser> {
		return Properties().apply {
			load(cookiesPath.toFile().inputStream())
		}.let { ps ->
			ps.keys.map { uid ->
				uid as String
				val cid = ps[uid] as String

				PaintUser(LuoGu(cid, uid).loggedUser)
			}
		}
	}

	@JvmStatic
	fun main(args : Array<String>) {
		while(true) loadFromCookies().drawFromImage(642, 0, ImageIO.read(imagePath.toFile()))
	}
}