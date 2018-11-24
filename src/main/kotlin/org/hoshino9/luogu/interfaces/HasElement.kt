package org.hoshino9.luogu.interfaces

import org.jsoup.nodes.Element
import org.jsoup.nodes.Node
import org.jsoup.select.Elements

interface HasElement {
	val elem : Element
}

interface HasElements {
	val elems : Elements
}

interface HasNodes {
	val nodes : List<Node>
}