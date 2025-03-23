package com.papauschek.ui

import com.papauschek.puzzle.{Puzzle, PuzzleConfig, PuzzleWords}
import com.papauschek.ui.{Globals, HtmlRenderer}
import org.scalajs.dom
import org.scalajs.dom.Worker
import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import upickle.default.*
import concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExport

/** the main user interface based on the `index.html` */
class MainPage:

  private var initialPuzzle: Puzzle = Puzzle.empty(PuzzleConfig())
  private var refinedPuzzle: Puzzle = initialPuzzle

  private var mainInputWords: Seq[String] = Nil

  private val inputElement = dom.document.getElementById("input").asInstanceOf[TextArea]
  private val outputPuzzleElement = dom.document.getElementById("output-puzzle")
  private val outputCluesElement = dom.document.getElementById("output-clues")
  private val resultInfoElement = dom.document.getElementById("result-info")
  private val outputJsonElement = dom.document.getElementById("output-json").asInstanceOf[TextArea]

  private val generateButton = dom.document.getElementById("generate-button").asInstanceOf[Button]
  private val generateSpinner = dom.document.getElementById("generate-spinner").asInstanceOf[Div]

  private val widthInputElement = dom.document.getElementById("width").asInstanceOf[Input]
  private val heightInputElement = dom.document.getElementById("height").asInstanceOf[Input]

  private val printButton = dom.document.getElementById("print-button").asInstanceOf[Button]
  private val replaceUnusedButton = dom.document.getElementById("replace-unused-button").asInstanceOf[Button]

  private val resultRow = dom.document.getElementById("result-row").asInstanceOf[Div]
  private val cluesRow = dom.document.getElementById("clues-row").asInstanceOf[Div]
  private val jsonRow = dom.document.getElementById("json-row").asInstanceOf[Div]
  private val downloadJsonButton = dom.document.getElementById("download-json-button").asInstanceOf[Button]
  
  generateButton.addEventListener("click", { _ => generateSolution() })
  printButton.addEventListener("click", { _ => printSolution() })
  downloadJsonButton.addEventListener("click", { _ => downloadJson() })
  replaceUnusedButton.addEventListener("click", { _ => replaceWithUnusedWords() })

  /** read the words from the user interface and generate the puzzle in the background using web workers */
  def generateSolution(): Unit =
    val rawInputWords = inputElement.value.linesIterator.map(normalizeWord).toSeq
    val inputWords = rawInputWords.filter(word => word.nonEmpty && !word.startsWith("#"))
    if (inputWords.nonEmpty) {
      mainInputWords = PuzzleWords.sortByBest(inputWords)
      val puzzleConfig = PuzzleConfig(
        width = widthInputElement.valueAsNumber.toInt,
        height = heightInputElement.valueAsNumber.toInt
      )
      generateSpinner.classList.remove("invisible")
      generateButton.classList.add("invisible")

      PuzzleGenerator.send(NewPuzzleMessage(puzzleConfig, mainInputWords)).map {
        puzzles =>
          generateSpinner.classList.add("invisible")
          generateButton.classList.remove("invisible")
          resultRow.classList.remove("invisible")
          cluesRow.classList.remove("invisible")
          jsonRow.classList.remove("invisible")
          initialPuzzle = puzzles.maxBy(_.density)
          refinedPuzzle = initialPuzzle
          renderSolution()
          generateJson()
      }.recover {
        case e: IllegalArgumentException =>
          generateSpinner.classList.add("invisible")
          generateButton.classList.remove("invisible")
          resultInfoElement.innerHTML = s"""<div class="alert alert-danger" role="alert">
            |  ${e.getMessage}
            |</div>""".stripMargin
          resultRow.classList.add("invisible")
          cluesRow.classList.add("invisible")
          jsonRow.classList.add("invisible")
      }
    }

  /** show the generated puzzle */
  def renderSolution(): Unit =
    outputPuzzleElement.innerHTML = HtmlRenderer.renderPuzzle(
      refinedPuzzle,
      showSolution = true)

    val unusedWords = mainInputWords.filterNot(refinedPuzzle.words.contains)
    val extraWords = refinedPuzzle.words -- initialPuzzle.words
    resultInfoElement.innerHTML = HtmlRenderer.renderPuzzleInfo(refinedPuzzle, unusedWords)
    outputCluesElement.innerHTML = HtmlRenderer.renderClues(refinedPuzzle, extraWords)

  /** show the print dialog */
  def printSolution(): Unit =
    dom.window.print()

  /** generate and display the JSON representation of the puzzle */
  private def generateJson(): Unit =
    outputJsonElement.value = HtmlRenderer.renderPuzzleJson(refinedPuzzle)

  /** download the JSON representation of the puzzle as a file */
  private def downloadJson(): Unit =
    val jsonContent = outputJsonElement.value
    if (jsonContent.nonEmpty) {
      val firstWord = refinedPuzzle.words.headOption.getOrElse("puzzle")
      val date = new scala.scalajs.js.Date()
      val dateStr = f"${date.getFullYear().toInt}-${(date.getMonth() + 1).toInt}%02d-${date.getDate().toInt}%02d"
      val filename = s"${firstWord}_${dateStr}.json"
      
      val blob = new org.scalajs.dom.Blob(scala.scalajs.js.Array(jsonContent), org.scalajs.dom.BlobPropertyBag(`type` = "application/json"))
      val url = org.scalajs.dom.URL.createObjectURL(blob)
      val link = dom.document.createElement("a").asInstanceOf[org.scalajs.dom.html.Anchor with org.scalajs.dom.raw.HTMLAnchorElement]
      link.href = url
      link.setAttribute("download", filename)
      link.click()
      org.scalajs.dom.URL.revokeObjectURL(url)
    }

  /** normalize words and expand german umlauts */
  private def normalizeWord(word: String): String =
    word.trim.toUpperCase.
      replace("Ä", "AE").
      replace("Ö", "OE").
      replace("Ü", "UE").
      replace("ß", "SS")

  /** replace the input with unused words from the current puzzle */
  private def replaceWithUnusedWords(): Unit =
    val unusedWords = mainInputWords.filterNot(refinedPuzzle.words.contains)
    if (unusedWords.nonEmpty) {
      inputElement.value = unusedWords.mkString("\n")
    }

