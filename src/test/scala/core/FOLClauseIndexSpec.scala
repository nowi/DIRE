package core

/**
 * User: nowi
 * Date: 19.12.2009
 * Time: 22:14:16
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{FOLClauseIndex, CNFClauseStore}
import domain.fol.ast._
import domain.fol.CriminalWestDomain
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


@RunWith(classOf[JUnit4Runner])
abstract class FOLClauseIndexSpec extends Spec with ShouldMatchers {
  val index: FOLClauseIndex
  describe("A FOLClause Index") {
    it("should insert and retrieve FOLClauses") {
      index.insert(CriminalWestDomain.C1)
      index.insert(CriminalWestDomain.C2)
      index.insert(CriminalWestDomain.C3)
      index.insert(CriminalWestDomain.C4)
      index.insert(CriminalWestDomain.C5)
      index.insert(CriminalWestDomain.C6)
      index.insert(CriminalWestDomain.C7)
      index.insert(CriminalWestDomain.C8)
      index.insert(CriminalWestDomain.goalClause)


      // retreive all clauses containing literal "American
      index.retrieve(Predicate("American", CriminalWestDomain.west)) should equal(Some(Set(CriminalWestDomain.C7)))
      index.retrieve(Predicate("American", CriminalWestDomain.west)) match {
        case Some(set) => set.size should equal(1)
        case None => assert(false)
      }


      // retreive all clauses containing literal criminal
      index.retrieve(Predicate("Criminal", CriminalWestDomain.west)) match {
        case Some(set) => set.size should equal(1)
        case None => assert(false)
      }
      index.retrieve(Predicate("Criminal", CriminalWestDomain.west)) should equal(
        Some(Set(CriminalWestDomain.C1)))


      // retreive all clauses containing literal -criminal
      index.retrieve(Negation(Predicate("Criminal", CriminalWestDomain.west))) match {
        case Some(set) => set.size should equal(1)
        case None => assert(false)
      }
      index.retrieve(Negation(Predicate("Criminal", CriminalWestDomain.west))) should equal(
        Some(Set(CriminalWestDomain.goalClause)))


    }

    it("should delete FOLClauses") {
      index.insert(CriminalWestDomain.C1)
      index.insert(CriminalWestDomain.C7)
      index.insert(CriminalWestDomain.goalClause)


      index.delete(CriminalWestDomain.C1)
      index.delete(CriminalWestDomain.C7)
      index.delete(CriminalWestDomain.goalClause)




      // retreive all clauses containing literal "American
      index.retrieve(Predicate("American", CriminalWestDomain.west)) should equal(None)


      // retreive all clauses containing literal criminal
      index.retrieve(Predicate("Criminal", CriminalWestDomain.west)) should equal(
        None)


      // retreive all clauses containing literal -criminal
      index.retrieve(Negation(Predicate("Criminal", CriminalWestDomain.west))) should equal(
        None)


    }
  }
}