package it.unibo.pps.ex2

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
