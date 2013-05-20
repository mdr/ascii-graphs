package com.github.mdr.ascii.layout.drawing

object BoxDrawingCharacters {

  def isBoxDrawingCharacter(c: Char): Boolean = c >= 0x2500 && c <= 0x257f

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