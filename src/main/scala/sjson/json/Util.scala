package sjson
package json

object Util {
  def quote(s: String): String = s match {
    case null => "null"
    case _ => {
      new StringBuilder(s.length + 2)
        .append('"')
        .append(s.foldLeft(new StringBuilder(""))((a, b) => a.append(escape(b, '"'))).toString)
        .append('"')
        .toString
    }
  }
  
  private def escape(c: Char, quoteChar: Char): String = c match {
    case '"' if (c == quoteChar) => "\\" + c
    case '"' => "" + c
    case '\'' if (c == quoteChar) => "\\" + c
    case '\'' => "" + c
    case '/' => "\\/"
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case x if (x < 32 || x > 126) => {
      val hex = Integer.toHexString(x)
      val len = hex.length
      "\\u" + (if (len < 4) "0000".substring(len) + hex else hex)
    }
    case _ => "" + c
  }
  
  import java.io._
  def readTillNl(in: InputStreamReader): String = {
    var c = -1
    var str = new StringBuffer
    do {
      c = in.read
      if (c != '\n' && c != -1) {
        str.append(c.toChar)
      }
    } 
    while (c != '\n' && c != -1)
    str.toString
  }

  def mkNum(v: Any, c: Class[_]) = {
    v match {
      case b: BigDecimal =>
        if (c.isAssignableFrom(classOf[Int])) b.intValue
        else if (c.isAssignableFrom(classOf[Long])) b.longValue
        else if (c.isAssignableFrom(classOf[Float])) b.floatValue
        else if (c.isAssignableFrom(classOf[Double])) b.doubleValue
        else if (c.isAssignableFrom(classOf[Short])) b.shortValue
        else b
      case _ => sys.error("unexpected")
    }
  }

  import java.util.Date
  def mkDate(v: String): Date = {
    new Date(v.toLong.longValue)
  }
}

