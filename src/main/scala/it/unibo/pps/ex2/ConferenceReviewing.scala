package it.unibo.pps.ex2

import it.unibo.pps.ex2.Question.{CONFIDENCE, FINAL}

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

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
      cr.reviews = Review(article, Map(Question.RELEVANCE -> relevance, Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin)) :: cr.reviews

    def orderedScores(article: Int, question: Question): List[Int] = cr.reviews.filter(review => review.article == article).map(review => review.scores(question)).sorted

    private def averageBy(article: Int, formula: Review => Double) = cr.reviews.foldLeft((0.0, 0))((acc, review) => if (review.article == article) (acc._1 + formula(review), acc._2 + 1) else acc) match
      case (sum, count) if count > 0 => sum / count
      case _ => 0.0

    def averageFinalScore(article: Int): Double = averageBy(article, r => r.scores(Question.FINAL))

    private def doesArticleHaveEnoughRelevance(article: Int): Boolean = cr.reviews.exists(review => review.article == article && review.scores(Question.RELEVANCE) >= 8.0)

    private def averageWeightedFinalScore(article: Int): Double = averageBy(article, r => r.scores(Question.FINAL) * r.scores(Question.CONFIDENCE) / 10.0)

    def acceptedArticles(): Set[Int] = cr.reviews.map(review => review.article).distinct.filter(article => doesArticleHaveEnoughRelevance(article) && averageFinalScore(article) >= 5.0).sorted.toSet

    def sortedAcceptedArticles(): List[Pair[Int, Double]] = acceptedArticles().toList.map(e => Pair(e, averageFinalScore(e))).sorted((r1, r2) => r1._2.compareTo(r2._2))

    def averageWeightedFinalScoreMap(): Map[Int, Double] = cr.reviews.map(review => review.article).distinct.map(e => (e, averageWeightedFinalScore(e))).toMap