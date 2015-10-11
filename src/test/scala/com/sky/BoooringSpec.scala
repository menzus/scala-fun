package com.sky

import java.io.File

import org.scalatest.{Matchers, FlatSpec}

class BoooringSpec extends FlatSpec with Matchers {
  "Boooring.allFiles" should "return all files recursively in the folder" in {
    Boooring.allFiles(new File("src/test/resources/filecount/dir2/dir21")) shouldBe
      List(new File("src/test/resources/filecount/dir2/dir21/dir211/file3"),
        new File("src/test/resources/filecount/dir2/dir21/file4"),
        new File("src/test/resources/filecount/dir2/dir21/file5")
      )
  }
}
