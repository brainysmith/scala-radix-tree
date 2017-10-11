package brainysmith

import org.scalatest._

class RadixTreeSpec extends FlatSpec with Matchers {

  "The RadixTree " should "insert correctly" in {
    val rt = RadixTree("tester" -> 1, "slow" -> 2)
      .insert("water", 3)
      .insert("slower", 4)
      .insert("test", 5)
      .insert("team", 6)
      .insert("toast", 7)
    val acc = rt.foldDepth(Seq[(String, Option[Int])]())((a, e) => a :+ e)
    acc shouldEqual List(("t", None), 
      ("e", None), 
      ("st", None), 
      ("er", Some(1)), 
      ("", Some(5)), 
      ("am", Some(6)), 
      ("oast", Some(7)), 
      ("slow", None), 
      ("", Some(2)), 
      ("er", Some(4)), 
      ("water", Some(3)))
  }

  "The RadixTree " should "search correctly" in {
    val rt = RadixTree("tester" -> 1, "slow" -> 2)
      .insert("water", 3)
      .insert("slower", 4)
      .insert("test", 5)
      .insert("team", 6)
      .insert("toast", 7)

      rt.lookup("none") shouldEqual None
      rt.lookup("toaster") shouldEqual None
      rt.lookup("tester") shouldEqual Some(1)
      rt.lookup("slow") shouldEqual Some(2)
      rt.lookup("water") shouldEqual Some(3)
      rt.lookup("slower") shouldEqual Some(4)
      rt.lookup("test") shouldEqual Some(5)
      rt.lookup("team") shouldEqual Some(6)
      rt.lookup("toast") shouldEqual Some(7)
  }

  "The RadixTree " should "search with prefix correctly" in {
    val rt = RadixTree("tester" -> 1, "slow" -> 2)
      .insert("water", 3)
      .insert("slower", 4)
      .insert("test", 5)
      .insert("team", 6)
      .insert("toast", 7)

      rt.findAllWithPrefix("none") shouldEqual List()
      rt.findAllWithPrefix("test") shouldEqual List(1, 5)
      rt.findAllWithPrefix("te") shouldEqual List(1, 5, 6)
      rt.findAllWithPrefix("t") shouldEqual List(1, 5, 6, 7)
      rt.findAllWithPrefix("") shouldEqual List(1, 5, 6, 7, 2, 4, 3)
  }

  "The RadixTree " should "delete correctly" in {
    val rt = RadixTree("tester" -> 1, "slow" -> 2)
      .insert("water", 3)
      .insert("slower", 4)
      .insert("test", 5)
      .insert("team", 6)
      .insert("toast", 7)

    val test1 = rt.remove("slower")
    test1.lookup("tester") shouldEqual Some(1)
    test1.lookup("slow") shouldEqual Some(2)
    test1.lookup("water") shouldEqual Some(3)
    test1.lookup("slower") shouldEqual None
    test1.lookup("test") shouldEqual Some(5)
    test1.lookup("team") shouldEqual Some(6)
    test1.lookup("toast") shouldEqual Some(7)

    val test2 = test1.remove("slow")
    test2.lookup("tester") shouldEqual Some(1)
    test2.lookup("slow") shouldEqual None
    test2.lookup("water") shouldEqual Some(3)
    test2.lookup("slower") shouldEqual None
    test2.lookup("test") shouldEqual Some(5)
    test2.lookup("team") shouldEqual Some(6)
    test2.lookup("toast") shouldEqual Some(7)

    val test3 = test2.remove("tester")
    test3.lookup("tester") shouldEqual None
    test3.lookup("slow") shouldEqual None
    test3.lookup("water") shouldEqual Some(3)
    test3.lookup("slower") shouldEqual None
    test3.lookup("test") shouldEqual Some(5)
    test3.lookup("team") shouldEqual Some(6)
    test3.lookup("toast") shouldEqual Some(7)

    val test4 = test3.remove("team")
    test4.lookup("tester") shouldEqual None
    test4.lookup("slow") shouldEqual None
    test4.lookup("water") shouldEqual Some(3)
    test4.lookup("slower") shouldEqual None
    test4.lookup("test") shouldEqual Some(5)
    test4.lookup("team") shouldEqual None
    test4.lookup("toast") shouldEqual Some(7)

    val test5 = test4.remove("toast")
    test5.lookup("tester") shouldEqual None
    test5.lookup("slow") shouldEqual None
    test5.lookup("water") shouldEqual Some(3)
    test5.lookup("slower") shouldEqual None
    test5.lookup("test") shouldEqual Some(5)
    test5.lookup("team") shouldEqual None
    test5.lookup("toast") shouldEqual None

    val test6 = test5.remove("test")
    test6.lookup("tester") shouldEqual None
    test6.lookup("slow") shouldEqual None
    test6.lookup("water") shouldEqual Some(3)
    test6.lookup("slower") shouldEqual None
    test6.lookup("test") shouldEqual None
    test6.lookup("team") shouldEqual None
    test6.lookup("toast") shouldEqual None

    val test7 = test6.remove("water")
    test7.lookup("tester") shouldEqual None
    test7.lookup("slow") shouldEqual None
    test7.lookup("water") shouldEqual None
    test7.lookup("slower") shouldEqual None
    test7.lookup("test") shouldEqual None
    test7.lookup("team") shouldEqual None
    test7.lookup("toast") shouldEqual None
  }
}
