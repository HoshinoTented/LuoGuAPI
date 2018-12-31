import java.awt.image.BufferedImage

inline fun iterateMatrixIndexed(width : Int, height : Int, action : (Int, Int) -> Unit) {
	(0 until width).forEach { x ->
		(0 until height).forEach { y ->
			action(x, y)
		}
	}
}

inline fun <T> iterateMatrix(width : Int, height : Int, matrix : T, action : (T, Int, Int) -> Unit) {
	iterateMatrixIndexed(width, height) { x, y ->
		action(matrix, x, y)
	}
}

inline fun BufferedImage.iterate(action : (BufferedImage, Int, Int) -> Unit) = iterateMatrix(width, height, this, action)