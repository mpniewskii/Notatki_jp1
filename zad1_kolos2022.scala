/*
  UWAGA! Uzupełnij kod funkcji „ranking” zgodnie z treścią zadania.
         Z poziomu SBT wydaj polecenie „test” żeby sprawdzić, czy
         Twoje rozwiązanie przechodzi przygotowane testy jednostkowe.
         Możesz dzięki temu nie tworzyć „programu głównego”.
*/

def ranking(): Any = {
  val input = io.Source
    .fromResource("test.txt")
    .getLines
    .map(_.split(" ").toList)
    .toList

    //write a function that groups input by columns, but ignores "id" column
    def groupByColumns(input: List[List[String]]): List[List[String]] = {
      val helper = input.map(el => el.tail)
      val grouped = helper.transpose
      grouped
    }
    
    val grouped = groupByColumns(input)

    //write a function that takes grouped and return a list of tuples (column number, sum of values)
    def average(grouped: List[List[String]]): List[(Int, Double)] = {
      val helper = grouped.map(el => el.map(el => el.toDouble))
      val helper2 = helper.map(el => (el.sum-el(0)))
      val helper3 = helper2.zipWithIndex
      val helper4 = helper3.map(el => (el._2 + 1, el._1))
      helper4
    }
    val averaged = average(grouped)
    val numberofids = input.drop(1).length
    //write a function that filters out columns with average value less than 0.5
    def filter(averaged: List[(Int, Double)]): List[(Int, Double)] = {
      val helper = averaged.filter(el => el._2/numberofids >= 0.5)
      helper
    }
    val filtered = filter(averaged)

    //write a function that sorts filtered by average value
    def sort(filtered: List[(Int, Double)]): List[(Int, Double)] = {
      val helper = filtered.sortBy(el => el._2)
      helper
    }    
    val sorted = sort(filtered).reverse
    //write a function that makes a ranking list from sorted and returns it as a (place, column number, average value) tuple with ex-aequo places
    def makeRanking(sorted: List[(Int, Double)]): List[(Int, Int, Double)] = {
      val helper = sorted.zipWithIndex
      val helper2 = helper.map(el => (el._2 + 1, el._1._1, el._1._2))
      helper2
    }
    val ranking = makeRanking(sorted)
    //write a function that takes ranking and makes ex-aequo places equal where third element is the same as the previous one using foldLeft and does not get missing places
    def exAequo(ranking: List[(Int, Int, Double)]): Any = {
      val helper = ranking.foldLeft(List[(Int, Int, Double)](), 1)({
        case ((acc, place), el) => 
        if (acc.isEmpty) {
          (acc :+ (place, el._2, el._3), place)
        } else {
          if (acc.last._3 == el._3) {
            (acc :+ (place, el._2, el._3), place)
          } else {
            (acc :+ (place+1, el._2, el._3), place + 1)
          }
        }
      })
      helper(0)
    }
    val exAequoRanking = exAequo(ranking)
    val finalRanking = exAequoRanking.asInstanceOf[List[(Int, Int, Double)]].map(el => (el._1, el._2))


    return finalRanking
    
}

@main
def kolos2022_zad1: Unit = {
  println(ranking())
}
