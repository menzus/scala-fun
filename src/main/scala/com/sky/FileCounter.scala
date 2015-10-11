package com.sky

import java.io.File

import scala.io.Source
import scala.util.Try

object Task1 {
  import FileCounter._

  def task(rootDir: String): Try[Int] = {
    allFilesInRoot(rootDir)
      .map(_.length)
  }
}

object Task2 {
  import FileCounter._
  import LineCountStats._

  def task(rootDir: String): Try[LineCountStats] = {
    allFilesInRoot(rootDir)
      .flatMap(lineCountStats)
      .map(fold)
  }

  private def lineCountStats(files: List[File]): Try[List[LineCountStats]] = {
    sequence {
      files.map { file =>
        Try(LineCountStats(files = 1, lines = Source.fromFile(file).getLines().length))
      }
    }
  }

  case class LineCountStats(files: Int, lines: Int)

  object LineCountStats {
    val zero = LineCountStats(0, 0)
    def plus(a: LineCountStats, b: LineCountStats) = LineCountStats(a.files + b.files, a.lines + b.lines)

    def fold(lineCountStats: List[LineCountStats]): LineCountStats = lineCountStats.foldLeft(zero)(plus)
  }
}

object Task3 {
  import FileCounter._
  import JavaFileStats._

  def task(rootDir: String): Try[JavaFileStats] = {
    allFilesInRoot(rootDir).map(_.filter(javaFile))
      .flatMap(javaFileStats)
      .map(fold)
  }
  
  private def javaFile(file: File) = file.getName.endsWith(".java")

  private def javaFileStats(files: List[File]): Try[List[JavaFileStats]] = {
    sequence {
      files.map(javaFileStats)
    }
  }
  
  private def javaFileStats(file: File): Try[JavaFileStats] = Try {
    Source.fromFile(file).getLines()
      .map { line =>
        if (line.trim isEmpty) whiteSpace
        else if (line.trim startsWith("//")) comment
        else codeLine
      }
      .foldLeft(zero)(plus)
  }

  case class JavaFileStats(whitespaces: Int, comments: Int, codeLines: Int)

  object JavaFileStats {
    val zero = JavaFileStats(0, 0, 0)
    def plus(a: JavaFileStats, b: JavaFileStats) = JavaFileStats(
      a.whitespaces + b.whitespaces, a.comments + b.comments, a.codeLines + b.codeLines)

    def fold(javaFileStats: List[JavaFileStats]): JavaFileStats = javaFileStats.foldLeft(zero)(plus)
    
    val whiteSpace = JavaFileStats(1, 0, 0)
    val comment = JavaFileStats(0, 1, 0)
    val codeLine= JavaFileStats(0, 0, 1)
  }
}

object FileCounter {
  def allFilesInRoot(root: String): Try[List[File]] = Try {
    allFiles(new File(root))
  }

  private def allFiles(file: File): List[File] = {
    if (file isDirectory) file.listFiles().map(allFiles(_)).flatten.toList else List(file)
  }

  def sequence[A](tries: List[Try[A]]): Try[List[A]] = {
    tries.foldRight(Try(List[A]())) { (t, ts) =>
      t.flatMap(v => ts.map(vs => v :: vs))
    }
  }
}
