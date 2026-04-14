package it.unibo.pps.ex2

import it.unibo.pps.ex2.Question.FINAL

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

/*object Question:
  private case class QuestionImpl(relevance: Int, significance: Int, confidence: Int, finalScore: Int)
  def apply(relevance: Int, significance: Int, confidence: Int, finalScore: Int): Question = QuestionImpl(relevance, significance, confidence, finalScore)*/

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  private case class Review(article: Int, scores: Map[Question, Int])
  private case class ConferenceReviewingImpl(var reviews: List[Review])
  opaque type ConferenceReviewing = ConferenceReviewingImpl

  def apply(): ConferenceReviewing = ConferenceReviewingImpl(List.empty)

  extension(cr: ConferenceReviewing)
    def loadReview(article: Int, scores: Map[Question, Int]): Unit = cr.reviews = Review(article, scores) :: cr.reviews
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = 
      val scores = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin
      )
      cr.reviews = Review(article, scores) :: cr.reviews

    def orderedScores(article: Int, question: Question): List[Int] = cr.reviews.filter(review => review.article == article).map(review => review.scores(question)).sorted

    def averageFinalScore(article: Int): Double = cr.reviews.foldLeft((0.0, 0))((acc, review) => if (review.article == article) (acc._1 + review.scores(Question.FINAL), acc._2 + 1) else acc) match
      case (sum, count) if count > 0 => sum / count
      case _ => 0.0

    private def doesArticleHaveEnoughRelevance(article: Int): Boolean = cr.reviews.exists(review => review.article == article && review.scores(Question.RELEVANCE) >= 8.0)

    def acceptedArticles(): Set[Int] = cr.reviews.map(review => review.article).distinct.filter(article => doesArticleHaveEnoughRelevance(article) && averageFinalScore(article) >= 5.0).sorted.toSet