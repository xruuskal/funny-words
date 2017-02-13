package nut

import scala.collection.mutable.{ ArrayBuffer }
import scala.io.Source
import java.io._

object FunnyWords {
  val vowels = "aeiouyAEIOUY"
  val scandicVowels = "åäöÅÄÖ"
  val wordRe = ("[" + vowels + scandicVowels + "]+").r
  var highScore = 0
  var funniestWords = new ArrayBuffer[String]()

  def main(args: Array[String]) {
    val lineRe = ("\\b[\\w" + scandicVowels + "]*[" + vowels + scandicVowels + "]{2,}[\\w" + scandicVowels + "]*\\b").r
    val fileName = args(0)
    if (isTextFile(fileName)) {
      for (line <- Source.fromFile(fileName)("UTF-8").getLines) {
        lineRe.findAllIn(line).foreach(countPoints _)
      }
    }
    else {
      val f = new File(fileName).listFiles.filter(x => isTextFile(x.getName))
      for (file <- f) {
        for (line <- Source.fromFile(file)("UTF-8").getLines) {
          lineRe.findAllIn(line).foreach(countPoints _)
        }
      }
    }
    println(s"Word(s) ${funniestWords.mkString(", ")} had the most points: $highScore")
  }

  def countPoints(word: String) {
    var totalScore = 0;
    wordRe.findAllIn(word).foreach(x => totalScore += formula(x.length))
    saveHighScore(word, totalScore)
  }

  def formula(chainLen: Int): Int = {
    return chainLen * Math.pow(2, chainLen).toInt
  }

  def saveHighScore(word: String, score: Int) {
    if (score > highScore) {
      funniestWords.clear()
      funniestWords += word
      highScore = score
    } else if (score == highScore) {
      funniestWords += word
    }
  }

  def isTextFile(file: String): Boolean = {
    return file.endsWith(".txt") || file.endsWith(".html")
  }
}
