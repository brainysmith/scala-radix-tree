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

}
