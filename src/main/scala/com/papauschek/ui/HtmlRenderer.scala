package com.papauschek.ui

import com.papauschek.puzzle.{Point, Puzzle}
import com.papauschek.service.{LLMService, ClueRequest}
import org.scalajs.dom
import upickle.default.{Writer, write, macroRW, ReadWriter}
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object HtmlRenderer:
  case class WordInfo(index: Int, vertical: Boolean, word: String)
  case class AnnotationInfo(x: Int, y: Int, words: Seq[WordInfo])
  case class ConfigInfo(width: Int, height: Int, wrapping: Boolean)
  case class PuzzleJson(
    grid: Array[String],
    clues: Map[String, String],
    difficulty: String
  )

  given ReadWriter[WordInfo] = macroRW[WordInfo]
  given ReadWriter[AnnotationInfo] = macroRW[AnnotationInfo]
  given ReadWriter[ConfigInfo] = macroRW[ConfigInfo]
  given ReadWriter[PuzzleJson] = macroRW[PuzzleJson]

  /** @return HTML for rendering a puzzle
   * @param puzzle the puzzle to render
   * @param showSolution if true, shows the solution (all characters) of the puzzle */
  def renderPuzzle(puzzle: Puzzle,
                   showSolution: Boolean = false): String =

    val annotation = puzzle.getAnnotation

    def renderCell(x: Int, y: Int): String =
      puzzle.getChar(x, y) match {
        case ' ' => ""
        case char =>
          val svgLetter = Option.when(showSolution) {
            s"""<text x="${x * 10 + 4.5}" y="${(y + 1) * 10 - 2}" text-anchor="middle" class="letter">$char</text>"""
          }

          val svgAnnotation = annotation.get(Point(x, y)) match {
            case Some(anno) if anno.nonEmpty =>
              val annotationIndices = anno.map(_.index).mkString(",")
              Some(s"""<text x="${x * 10 + 0.8}" y="${(y + 1) * 10 - 0.8}" class="annotation">$annotationIndices</text>""")
            case _ => None
          }

          val svgCell = s"""<rect x="${x * 10}" y="${y * 10}" rx="0.5" ry="0.5" width="10" height="10"
            |  style="fill:white;stroke:black;stroke-width:0.3" />""".stripMargin

          svgCell + svgAnnotation.mkString + svgLetter.mkString
      }

    def renderHeight(y: Int): String =
      (0 until puzzle.config.width).map(renderCell(_, y)).mkString("\r\n")

    val renderedPuzzle = (0 until puzzle.config.height).map(renderHeight).mkString("\r\n")

    s"""<svg viewBox="-1 -1 ${puzzle.config.width * 10 + 2} ${puzzle.config.height * 10 + 2}">
      |  <style>
      |    .annotation {
      |      font: 3px sans-serif;
      |      fill: #999999;
      |    }
      |    .letter {
      |      font: 8px sans-serif;
      |      fill: black;
      |    }
      |  </style>
      |  $renderedPuzzle
      |</svg>""".stripMargin


  /** @return HTML representing the clues (= solution) words for this puzzle */
  def renderClues(puzzle: Puzzle, extraWords: Set[String]): String =

    val annotation = puzzle.getAnnotation
    val sortedAnnotationValues = annotation.values.flatten.toSeq.sortBy(_.index)

    def renderDescriptions(vertical: Boolean): String = {
      sortedAnnotationValues.filter(_.vertical == vertical).map {
        p =>
          val formattedWord = if (extraWords.contains(p.word)) s"<strong>${p.word}</strong>" else p.word
          "<div>" + p.index + ") " + formattedWord + "</div>"
      }.mkString("\r\n")
    }

    s"""<div class="row">
       |  <div class="col-lg-6">
       |    <h4>Horizontal</h4>
       |    <p>${renderDescriptions(vertical = false)}</p>
       |  </div>
       |  <div class="col-lg-6">
       |    <h4>Vertical</h4>
       |    <p>${renderDescriptions(vertical = true)}</p>
       |  </div>
       |</div>
       |""".stripMargin


  /** @return HTML representing some additional info about the puzzle, such as density and discarded words. */
  def renderPuzzleInfo(puzzle: Puzzle, unusedWords: Seq[String]): String =
    val infoText = s"This puzzle has a <strong>density of ${(puzzle.density * 100).round}%</strong>. " +
      s"This is the area covered by letters. " +
      s"If you prefer a more dense puzzle, add more words to the list above and let the tool discard the words that do not fit well. "
    val unusedInfoText = Option.when(unusedWords.nonEmpty)(s"The following words from your list were NOT used: ${unusedWords.mkString(", ")}").mkString
    infoText + unusedInfoText


  /** @return JSON representation of the puzzle including grid and clues */
  def renderPuzzleJson(puzzle: Puzzle, llmService: Option[LLMService] = None): Future[String] = {
    val grid = (0 until puzzle.config.height).map { y =>
      (0 until puzzle.config.width).map { x =>
        puzzle.getChar(x, y) match {
          case ' ' => '.'
          case char => char
        }
      }.mkString
    }.toArray

    val annotation = puzzle.getAnnotation
    val wordInfos = annotation.values.flatten.toSeq

    // If LLM service is provided, generate clues for each word
    val cluesFuture = llmService match {
      case Some(service) =>
        // Generate clues for all words in parallel
        Future.sequence(wordInfos.map { wordInfo =>
          val difficulty = if (puzzle.density > 0.8) "Hard" else if (puzzle.density > 0.6) "Medium" else "Easy"
          service.generateClue(ClueRequest(wordInfo.word, difficulty))
            .map(response => wordInfo.word -> response.clue)
        }).map(_.toMap)
      case None =>
        // Fallback to placeholder clues if no LLM service
        Future.successful(wordInfos.map { wordInfo =>
          wordInfo.word -> s"Clue for ${wordInfo.word}"
        }.toMap)
    }

    val difficulty = if (puzzle.density > 0.8) "Hard" else if (puzzle.density > 0.6) "Medium" else "Easy"

    cluesFuture.map { clues =>
      val result = PuzzleJson(
        grid = grid,
        clues = clues,
        difficulty = difficulty
      )
      write(result, indent = 2)
    }
  }

