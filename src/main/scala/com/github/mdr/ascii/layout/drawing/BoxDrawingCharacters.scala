package com.github.mdr.ascii.layout.drawing

object BoxDrawingCharacters {

  def connectSingleRight(c: Char) = c match {
    case '│' ⇒ '├'
    case '─' ⇒ '─'

    case '║' ⇒ '╟'
    //    case '═' ⇒ '═'

    case '╢' ⇒ '╫'
    case '╟' ⇒ '╟'
    //    case '╤' ⇒ '╤'
    //    case '╧' ⇒ '╧'

    case '╫' ⇒ '╫'
    //    case '╪' ⇒ '╪'

    //    case '╡' ⇒ '╡'
    case '╨' ⇒ '╨'
    //    case '╞' ⇒ '╞'
    case '╥' ⇒ '╥'

    case '┼' ⇒ '┼'

    case '┐' ⇒ '┬'
    case '┘' ⇒ '┴'
    case '└' ⇒ '└'
    case '┌' ⇒ '┌'

    case '┬' ⇒ '┬'
    case '┴' ⇒ '┴'
    case '┤' ⇒ '┼'
    case '├' ⇒ '├'
  }

}