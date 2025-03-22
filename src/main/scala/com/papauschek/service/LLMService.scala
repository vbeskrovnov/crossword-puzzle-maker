package com.papauschek.service

import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import ujson._

case class ClueRequest(word: String, difficulty: String)
case class ClueResponse(clue: String)

class LLMService(apiKey: String) {
  private val apiEndpoint = "https://api-inference.huggingface.co/models/mistralai/Mistral-7B-Instruct-v0.2"
  
  def generateClue(request: ClueRequest): Future[ClueResponse] = {
    val prompt = s"""<s>[INST]
You are an expert at creating crossword puzzle clues. Please generate a single, concise clue for the word "${request.word}" without using that word itself or any direct synonyms. 
Your final response must be valid JSON and contain exactly one top-level field named "clue" whose value is the clue text.

Example output:
{
  "clue": "Lively strummer's instrument"
}
[/INST]
"""
    
    val requestData = ujson.Obj(
      "inputs" -> prompt,
      "parameters" -> ujson.Obj(
        "max_new_tokens" -> 100,
        "temperature" -> 0.7,
        "return_full_text" -> false
      )
    ).toString

    // Log the request
    dom.console.log(s"Request to Hugging Face API for word '${request.word}':")
    dom.console.log(s"Prompt: $prompt")
    dom.console.log(s"Request data: $requestData")

    Ajax.post(
      url = apiEndpoint,
      data = requestData,
      headers = Map(
        "Authorization" -> s"Bearer $apiKey",
        "Content-Type" -> "application/json"
      )
    ).map { xhr =>
      // Log the raw response
      dom.console.log(s"Raw response from Hugging Face API for word '${request.word}':")
      dom.console.log(xhr.responseText)

      val jsonResponse = ujson.read(xhr.responseText)
      val generatedText = jsonResponse(0)("generated_text").str
      val clueJson = ujson.read(generatedText)
      
      // Log the parsed response
      dom.console.log(s"Parsed response for word '${request.word}':")
      dom.console.log(s"Generated text: $generatedText")
      dom.console.log(s"Parsed JSON: $clueJson")
      
      ClueResponse(
        clue = clueJson("clue").str
      )
    }.recover { case e =>
      // Log any errors
      dom.console.error(s"Error generating clue for word '${request.word}':")
      dom.console.error(e.getMessage)
      throw e
    }
  }
} 