package main.chapter3

import main.Test

/**
  * Created by adrian on 25/12/2016.
  */
class TestTree extends Test {

   "tree size" should "return size" in {
      Tree.size(Leaf(1)) shouldBe 1
      Tree.size(Node(Leaf(1), Leaf(2) )) shouldBe 3

      //      are these valid trees ?
      //      Tree.size(Node(Leaf(1), null)) shouldBe 2
      //      Tree.size(Node(null, Leaf(1))) shouldBe 2

   }

   "tree sizeF" should "return size" in {
      Tree.sizeF(Leaf(1)) shouldBe 1
      Tree.sizeF(Node(Leaf(1), Leaf(2) )) shouldBe 3

   }

   "max" should "return max" in {
      Tree.max(Leaf(1)) shouldBe 1
      Tree.max(Node(Leaf(1), Leaf(2) )) shouldBe 2
      Tree.max(Node(Leaf(2), Leaf(1) )) shouldBe 2
      Tree.max(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      )) shouldBe 3
   }

   "maxF" should "return max" in {
      Tree.maxF(Leaf(1)) shouldBe 1
      Tree.maxF(Node(Leaf(1), Leaf(2) )) shouldBe 2
      Tree.maxF(Node(Leaf(2), Leaf(1) )) shouldBe 2
      Tree.maxF(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      )) shouldBe 3
   }

   "depth" should "return depth" in {
      Tree.depth(Leaf(1)) shouldBe 0
      Tree.depth(Node(Leaf(1), Leaf(2) )) shouldBe 1
      Tree.depth(Node(Leaf(2), Leaf(1) )) shouldBe 1
      Tree.depth(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      )) shouldBe 2
   }

   "depthF" should "return depth" in {
      Tree.depthF(Leaf(1)) shouldBe 0
      Tree.depthF(Node(Leaf(1), Leaf(2) )) shouldBe 1
      Tree.depthF(Node(Leaf(2), Leaf(1) )) shouldBe 1
      Tree.depthF(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      )) shouldBe 2
   }

   "map" should "apply f to every element" in {
      Tree.map(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      ))(a => a + 2) shouldBe
         Node(Leaf(5),
            Node(Leaf(4), Leaf(3))
         )
   }

   "mapF" should "apply f to every element" in {
      Tree.mapF(Node(Leaf(3),
         Node(Leaf(2), Leaf(1))
      ))(a => a + 2) shouldBe
         Node(Leaf(5),
            Node(Leaf(4), Leaf(3))
         )
   }
}
