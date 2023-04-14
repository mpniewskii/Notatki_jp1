
case class CountryData(
  CountryName: String,
  Year: String,
  LadderScore: String,
  LogGDPPerCapita: String,
  SocialSupport: String,
  HealthyLifeExpectancyAtBirth: String,
  FreedomToMakeLifeChoices: String,
  Generosity: String,
  PerceptionsOfCorruption: String,
  PositiveAffect: String,
  NegativeAffect: String
)

/*
    Korzystając wyłącznie z mechanizmów kolekcji języka Scala znajdź kraj o najdłużej rosnącym wskaźniku LadderScore.
    Innymi słowy, korzystając z załączonych danych szukamy kraju, dla którego wskaźnik LadderScore najdłużej
    utrzymał „dobrą passę” (z roku na rok się zwiększał).

    Zwróć uwagę na to, że w danych mogą wystąpić „linie” z brakującymi danymi. Takie linie powinny zostać
    POMINIĘTE. Brakujące dane oznaczają, że w linii występują sekwencje postaci: ,,, przykładowo:

        Kosovo,2020,6.294,,0.792,,0.880,,0.910,0.726,0.201

    Linie takie, jako „niewiarygodne” należy pominąć (oczywiście nie zmieniając samego pliku danych)
    zanim program rozpocznie analizę.

    W rozwiązaniu nie wolno używać zmiennych, ani konstrukcji imperatywnych, takich jak pętle
*/

def longestIncreasingSubsequence(data: List[(String, String, Double)]): Int = {
  val diffs = data.sliding(2).map { case List((_, _, score1), (_, _, score2)) => score2 - score1 }.toList
  val (_, maxLength) = diffs.foldLeft((1, 1)) {
    case ((currentLength, maxLength), diff) =>
      val newLength = if (diff > 0) currentLength + 1 else 1
      (newLength, maxLength.max(newLength))
  }
  maxLength
}



@main
def kolos2021_zad4(): Unit = {
  val results = io.Source
    .fromResource("world-happiness-report.txt")
    .getLines
    .map(line => line.split(',').toList)
    .toList

  val filtering = results.filter(_.exists(_.isEmpty) == false)
  val helper = filtering.map(el => (el(0), el(1), el(2).toDouble))
  val grupping = helper.groupBy(el => el(0))
  val sortedGrupping = grupping.map(el => el(1).sortBy(el => el(1)))
  val zebybylylisty = sortedGrupping.map(el => el.map(el => el))
  val tester = zebybylylisty.map(el => (el(0)(0), el.foldLeft((0, 0.0, 0)) { case  ((streak, prevVal, maxStreak), (_, _, currVal)) =>
  if (currVal > prevVal) {
    (streak + 1, currVal, maxStreak.max(streak + 1))
  } else {
    (1, currVal, maxStreak)}
  }
  ))

  // write a function that returns the longest streak of increasing values and the country name

  val longestStreak = tester.maxBy(_._2._3)
  println(longestStreak)
  


   
  

  

  
  


}