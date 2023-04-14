/*
    Korzystając wyłącznie z operacji na kolekcjach (w szczególności nie możesz użyć rekurencji
    ani mechanizmów imperatywnych, takich jak zmienne i pętle) zdefiniuj funkcję:
        def findPairs(n: Int): Set[(Int,Int)]
    taką, że dowolnej liczby całkowitej N > 1
        findPairs(N)
    zawiera wszystkie pary postaci (p1, p2), gdzie p1 i p2 są liczbami
    pierwszymi takimi, że p1 + p2 = 2 * N oraz p1 <= p2.
*/

def findPairs(n: Int): Set[(Int,Int)] = {

    val primeList = (2 to 2*n).foldLeft(List[Int]())((acc, el) => {
        if (acc.forall(el % _ != 0)) acc :+ el
        else acc
    })

    val pairsList = primeList.foldLeft(List[(Int, Int)]())((acc, el) => {
        if (primeList.contains(2 * n - el)) acc :+ (el, 2 * n - el)
        else acc
    })

    val helper = pairsList.foldLeft(List[(Int, Int)]())((acc, el) => {
        if (el._1 <= el._2) acc :+ el
        else acc
    })

    return helper.toSet
    
}


@main
def kolos2021_zad5(): Unit = {
   println(findPairs(50))
}

