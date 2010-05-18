package core.index

/**
 * User: nowi
 * Date: 15.04.2010
 * Time: 10:11:57
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import domain.fol.Substitution
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class SubstitutionTreeIndexSpec extends Spec with ShouldMatchers with Logging {
  describe("A Index") {
    val a = Constant("a")
    val b = Constant("b")
    val c = Constant("c")
    val d = Constant("d")
    val g = (node: FOLNode) => Function("g", node)
    val f = (node1: FOLNode, node2: FOLNode) => Function("f", node1, node2)
    val y = Variable("y")
    val z = Variable("z")
    val u = Variable("u")

    val x = Variable("x")
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    it("should insert the insertion sequance from page 174 term indexing") {
      // insert ( x --> f(,z,g(b)) ) into empty tree , should trigger rule 6.14
      val emtpyTree: SubstitutionIndexTree = EmptyTree()

      val A = emtpyTree.insert(f(z, g(b)))
      (A.substitution match {
        case Some(s) => {
          log.info("Substitution at A is %s", s)
          true
        }
        case _ => false

      }) should be(true)


      val B = A.insert(f(y, g(b)))
      (B match {
        case LeafNode(s, _,_) => {
          log.info("Substitution at B is %s", s)
          true
        }
        case _ => false
      }) should be(true)


      val C = B.insert(f(a, b))
      (C match {
        case InnerNode(s, children,_) => {
          log.info("Substitution at C is %s", s)
          log.info("Children at C are %s", children)
          true
        }
        case _ => {
          log.info("C is %s", C)
          false
        }

      }) should be(true)



      val D = C.insert(f(c, g(d)))
      (D match {
        case InnerNode(s, children,_) => {
          log.info("Substitution at D is %s", s)
          log.info("Children at D are %s", children)
          true
        }
        case _ => {
          log.info("D is %s", D)
          false
        }

      }) should be(true)



      val E = D.insert(f(b, g(a)))
      (E match {
      // check the structure of the tree only for now...
      // InnerNode(root)
      //  -- InnerNode
      //    --LeafNode
      //    --LeafNode
      //    --LeafNode
      //  -- LeafNode
        case InnerNode(_, List(InnerNode(_, List(LeafNode(_, _,_), LeafNode(_, _,_), LeafNode(_, _,_)),_), LeafNode(_, _,_)),_) => {
          //log.info("Substitution at E is %s", s)
          //log.info("Children at E are %s", children)
          true
        }
        case _ => {
          log.info("E is %s", E)
          false
        }

      }) should be(true)

    }


    it("should insert a Fucntion nested inside a Predicate this is a example from the conference dataset") {
      val skf0213 = (t1: FOLNode) => Function("skf0213", t1)
      val Small = (t1: FOLNode) => Predicate("Small", t1)
      val u = Variable("U")

       // insert ( x --> f(,z,g(b)) ) into empty tree , should trigger rule 6.14
      val emtpyTree: SubstitutionIndexTree = EmptyTree()

      val A = emtpyTree.insert(Small(u))
      (A.substitution match {
        case Some(s) => {
          log.info("Substitution at A is %s", s)
          log.info("Tree A : is %s", A)
          true
        }
        case _ => false

      }) should be(true)

      val B = A.insert(Small(skf0213(u)))
      (B.substitution match {
        case Some(s) => {
          log.info("Substitution at B is %s", s)
          log.info("Tree B : is %s", B)
          true
        }
        case _ => false

      }) should be(true)
            




    }






    // TODO reenable this test after fixed variable creation
    it("insertion should create a linar substitution tree. see page 191") {
      var root: SubstitutionIndexTree = EmptyTree()
      root = root.insert(f(a, a))
      root = root.insert(f(b, b))
      root = root.insert(f(a, b))

//      root.toString should equal("InnerNode(Map(u -> f(x_238,x_240)),List(InnerNode(Map(x_238 -> a),List(LeafNode(Map(x_240 -> a)), LeafNode(Map(x_240 -> b)))), LeafNode(Map(x_238 -> b, x_240 -> b))))")

      log.info("Root is %s" format (root))

      true
    }


    // TODO reenable this test after fixed variable creation
    it("should cope with empty substitutions in the tree") {
      log.info("should cope with empty substitutions in the tree")
      var root: SubstitutionIndexTree = EmptyTree()
      root = root.insert(g(a))
      root = root.insert(a)
      root.toString should equal("LeafNode(Map(x -> g(a), y -> a))")
      root = root.insert(g(b))
      root = root.insert(b)
      root.toString should equal("InnerNode(Map(x -> g(x_20)),List(LeafNode(Map(x_20 -> a, y -> a)), LeafNode(Map(x_20 -> b, z -> b))))")

      root = root.insert(a)
      root.toString should equal("InnerNode(Map(),List(InnerNode(Map(x -> g(x_20)),List(LeafNode(Map(x_20 -> a, y -> a)), LeafNode(Map(x_20 -> b, z -> b)))), LeafNode(Map(z -> a))))")

      root = root.insert(g(a))
      root.toString should equal("InnerNode(Map(),List(InnerNode(Map(x -> g(x_20)),List(InnerNode(Map(x_20 -> a),List(LeafNode(Map(y -> a)), LeafNode(Map()))), LeafNode(Map(x_20 -> b, z -> b)))), LeafNode(Map(z -> a))))")
      log.info("Root is %s" format (root))

      true
    }


    // TODO fix pattern matching here

//    it("should retrieve from stated tree like on page 159") {
//      log.info("should retrieve like in the example from page 171")
//      // create the tree from page
//      var root: SubstitutionIndexTree = InnerNode(Map(u -> f(x1, x2)), List(InnerNode(Map(x2 -> b), List(LeafNode(Map(x1 -> a), Nil,_), LeafNode(Map(x1 -> IndicatorVariable(0)), Nil,_)),_), LeafNode(Map(x1 -> b, x2 -> IndicatorVariable(0)), Nil,_)),_)
//
//      // we now search for substitutions that are compatible with { u -> f(a,x) }
//      // we search for substitutions r such tat ur is unifiable with f(a,x)
//
//      // we begin by binding the variable u to the term f(a,x)
//      val query = Substitution(Map(u -> f(a, x)))
//      val result = root.retrieveUnifiable(query)
//      log.info("Result is %s" format (result))
//      log.info("Nodes retrieved  %s" format (result.map(_.substitution.get)))
//      result.size should equal(4)
//
//      true
//
//    }


    it("should retrieve like in the example from page 171") {
      log.info("should retrieve like in the example from page 171")
      // create the tree from page
      var root: SubstitutionIndexTree = EmptyTree()
      root = root.insert(f(x, x))
      root = root.insert(f(z, z))
      root = root.insert(f(x, y))
      root = root.insert(f(a, g(z)))
      root = root.insert(f(g(x), g(x)))
      root = root.insert(f(g(d), g(x)))
      log.info("Root is %s" format (root))
      true

    }

  }


}
