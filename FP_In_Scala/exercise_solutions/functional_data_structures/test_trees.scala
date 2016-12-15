import fp.functional_data_structures.Tree;
import fp.functional_data_structures.Branch;
import fp.functional_data_structures.Leaf;

object test_trees {
    def main(args:Array[String]):Unit = {
        val b1:Tree[Int] = Branch(Leaf(91), Leaf(90))
        val b2:Tree[Int] = Branch(Leaf(3), Leaf(4))
        val t1:Tree[Int] = Branch(b1, b2)
        val s1 = Tree.size(t1)
        val m1 = Tree.maximum(t1)
        val d1 = Tree.depth(t1)
        println(s"Tree 1 stats $s1 $m1 $d1")
        val b3:Tree[Int] = Branch(Leaf(5),Leaf(6))
        val b4:Tree[Int] = Branch(Leaf(100), b2)
        val t2:Tree[Int] = Branch(b4, b1)
        val s2 = Tree.size(t2)
        val m2 = Tree.maximum(t2)
        val d2 = Tree.depth(t2)
        println(s"Tree 2 stats $s2 $m2 $d2")
        val map1 = Tree.map(t2)((x) => x * x)
        println(s"Map result $map1")

        val s3 = Tree.sizeviaFold(t1)
        val s4 = Tree.sizeviaFold(t2)
        println(s"Size stats using fold $s3\t$s4")
        val m3 = Tree.maximumviaFold(t1)
        val m4 = Tree.maximumviaFold(t2)
        println(s"Max stats using fold $m3\t$m4")
        val d3 = Tree.depthviaFold(t1)
        val d4 = Tree.depthviaFold(t2)
        println(s"Depth stats using fold $d3\t$d4")
        val map2 = Tree.map(t2)(x => x * x)
        println(s"Map stats using fold $map2")
    }
}