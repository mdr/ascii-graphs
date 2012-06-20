package com.github.mdr.ascii.util

object Utils {

 def transformValues[K, V, V2](map: Map[K, V])(f: V ⇒ V2): Map[K, V2] = map.map { case (k, v) ⇒ (k, f(v)) }

}