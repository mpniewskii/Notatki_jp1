/*
  Zadanie 1. Stwórz definicję funkcji

  def znajdź[A,B](alist: List[A], blist: List[B])(pred: Pred[(A,B)]): Set[(A,B)] = { ... }

  tak, aby jej wynikiem był zbiór wszystkich par (a,b) takich, że a i b są dowolnymi elementami
  list „alist” i „blist” odpowiednio, dla których zachodzi pred(a,b).

  W rozwiązaniu skorzystaj z rekurencji ogonowej (pmiętaj o użyciu adnotacji
  „@annotation.tailrec”!) i dopasowania wzorców. Nie używaj żadnych operacji
  na kolekcjach poza „::” oraz „++” (dodawanie zbiorów).

  WARTOŚĆ ZADANIA: 2 pkt
*/



 def znajdz[A,B](alist: List[A], blist: List[B])(pred: (Int, Int) => Boolean): Any = { 

    def makePairs[A,B](alist: List[A], blist: List[B], acc: List[(A,B)] = List()): List[(A,B)] = (alist, blist) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (h1 :: t1, h2 :: t2) => makePairs(t1, blist, acc :+ (h1, h2)) ++ makePairs(alist, t2, acc :+ (h1, h2))
    }

    def filterPairs[A,B](pairs: List[(A,B)], pred: (Int, Int) => Boolean, acc: List[(A,B)] = List()): List[(A,B)] = pairs match {
      case Nil => acc
      case (h1, h2) :: t => if(pred(h1.asInstanceOf[Int], h2.asInstanceOf[Int])) filterPairs(t, pred, acc :+ (h1, h2)) else filterPairs(t, pred, acc)
    }
    filterPairs(makePairs(alist, blist), pred)
    
}
 
  


    @main
    def kolos2020_zad1(): Unit={
        println(znajdz(List(2,3,4,4,3,2), List(4, 2, 0, 6, 9))((x: Int, y: Int) => x/(y+1)>x-3))}
