package com.sky

import java.io.File

import scala.io.Source

object Boooring {
  def task1(root: String): Int = {
    allFiles(new File(root)).size
  }

  def task2(root: String): Int = {
    def lineCount(file: File): Int = Source.fromFile(file).getLines().size

    allFiles(new File(root))
      .map(lineCount)
      .sum
  }

  case class JavaFileStats(whiteSpaces: Int, comments: Int, codeLines: Int)

  object JavaFileStats {
    def task(root: String) =
      allFiles(new File(root))
        .filter(javaFile)
        .map(JavaFileStats.toStats)
        .foldLeft(zero)(plus)

    def zero = JavaFileStats(0, 0, 0)

    def plus(a: JavaFileStats, b: JavaFileStats) = {
      JavaFileStats(a.whiteSpaces + b.whiteSpaces, a.comments + b.comments, a.codeLines + b.codeLines)
    }

    private def javaFile(file: File) = file.getName.endsWith(".java")

    private def apply(line: String): JavaFileStats = {
      if (line.trim isEmpty) JavaFileStats(1, 0, 0)
      else if (line.trim startsWith("//")) JavaFileStats(0, 1, 0)
      else JavaFileStats(0, 0, 1)
    }

    private def toStats(file: File): JavaFileStats = {
      Source.fromFile(file).getLines().map(apply).foldLeft(zero)(plus)
    }

  }

  def allFiles(file: File): List[File] = {
    if (file isDirectory) file.listFiles().map(allFiles(_)).flatten.toList else List(file)
  }
}
