package org.scalafmt.util

/**
  * Shamelessly copied from:
  * http://stackoverflow.com/a/7542476
  */
object Tabulator {

  def csv(table: Seq[Seq[Any]]) = table.map(_.mkString(",")).mkString("\n")

  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table)
        yield
          (for (cell <- row)
            yield
              if (cell == null) 0
              else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String =
    (rowSeparator :: rows.head :: rowSeparator :: rows.tail.toList ::: rowSeparator :: List(
      )).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes))
      yield
        if (size == 0) ""
        else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) =
    colSizes map {
      "-" * _
    } mkString ("+", "+", "+")
}
