package util

object Util {

  def textInBox(text: String): String = {
    s"""╭${"─" * (text.length + 2)}╮
       |│ $text │
       |╰${"─" * (text.length + 2)}╯
     """.stripMargin
  }

  implicit class StringOps(v: String) {
    def inBox(): String = textInBox(v)
  }
}
