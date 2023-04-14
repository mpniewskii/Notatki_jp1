/*
  Zadanie 2. Korzystając z mechanizmów kolekcji języka Scala zdefiniuj funkcje

  def stwórzIndeks(): Map[String, List[Int]]

  def słowa(indeks: Map[String, List[Int]], nrLinii: Int): Set[String]

  Pierwsza z nich powinna utworzyć indeks wszystkich słów występujących w pliku
  „tekst.txt”. Każdemu słowu należy przypisać listę wszystkich numerów linii, w których
  ono występuje. W ramach takiej listy numery linii powinny być uporządkowane rosnąco
  nie mogą się powtarzać. Budując indeks traktuj wielkie i małe litery identycznie!

  Druga z funkcji, na bazie zbudowanego przez stwórzIndeks() indeksu (parametr indeks),
  powinna dla podanego numeru linii (parametr nrLinii) zwrócić zbiór wszystkich słów
  występujących w tej linii. UWAGA! Funkcja „słowa” musi korzystać z indeksu, a nie
  z pliku „tekst.txt”!

  Rozwiązując zadanie wykorzystaj JEDYNIE mechanizmy kolekcji – w szczególności nie
  definiuj żadnej funkcji rekurencyjnie!!!

  WARTOŚĆ ZADANIA: 3 pkt
*/

  // funkcja nie odfiltrowuje znaków, które nie są literami
  // (jak np. znaki interpunkcyjne)
  def stworzIndeks():  Map[String, List[Int]]= {
    val data = io.Source.fromResource("tekst.txt")
      .getLines
      .map(_.toLowerCase)
      .toList
      .zipWithIndex
      .flatMap{case (line, index) => line.split(" ").map(word => (word, index))}
      .groupBy(_._1)
      .mapValues(_.map(_._2).sorted).toMap
    val words = data
    return words
      
  }
  def słowa(indeks: Map[String, List[Int]], nrLinii: Int): Any = {
    val words = indeks
    val line = nrLinii
    val wordsOnLine = words.filter(_._2.contains(line))
    .map(_._1).filter(_.forall(_.isLetter)).filter(_.length > 0)
    return wordsOnLine
  }


  
@main
  def kolos2020_zad2(): Unit = {
    println(słowa(stworzIndeks(), 2))
  }  





