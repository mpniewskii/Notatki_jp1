import scala.annotation.tailrec

/*
    Wykorzystując rekurencję (wyłącznie ogonową) zdefiniuj funkcję:

        def countResults[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): Set[(C, Int)]

    która zaaplikuje funkcję „f” do elementów l1(i), l2(i), gdzie 0 <= i < min(l1.length, l2.length)
    oraz zwróci zbiór składający się z par:

        (wynik funkcji f, liczba par dla których f zwróciła dany wynik).

    Przykładowo dla:

        countResults(List(1,2,3), List(4,5,4,6))(_+_) == Set((5,1), (7,2))

    ponieważ: 1+4 = 5, 2+5 = 7, 4+3 = 7, 6 pomijamy bo to „nadmiarowy” element w drugiej z list.

    W rozwiązaniu należy skorzystać z mechanizmu dopasowania do wzorca (pattern matching).
    Nie używaj zmiennych ani „pętli” (while, for bez yield, foreach).
*/

def countResults[A, B, C](arr1: List[A], arr2: List[B])(helper: (A, B) => C): Any = {

    def robto(arr1: List[A], arr2: List[B], akum: List[C] = List())(helper: (A, B) => C): Any = (arr1, arr2) match{
        case (Nil, Nil) => akum.groupBy(el => el).toList.map((el1, el2) => (el1, el2.length))
        case (Nil, List(_)) => akum.groupBy(el => el).toList.map((el1, el2) => (el1, el2.length))
        case (List(_), Nil) => akum.groupBy(el => el).toList.map((el1, el2) => (el1, el2.length))
        case (h1 :: t1, h2 :: t2) => robto(t1, t2, akum :+ helper(h1, h2))(helper)
    }

    robto(arr1, arr2)(helper)

}

@main
def kolos2021_zad1(): Unit = {
  println(countResults(List(1, 2, 3), List(4, 5, 4, 6))(_ + _))
}