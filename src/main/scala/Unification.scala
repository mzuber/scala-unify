/*
 * Copyright (c) 2014, Martin Zuber
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import org.kiama.rewriting.Rewriter._
import scala.reflect.ClassTag

import scala.collection.Map.empty


/**
  *
  */
trait Unification {

  /**
    * The type for variables, i.e., the meta-level terms.
    */
  type Variable


  /**
    * Try to unify the two given terms.
    *
    * @return The most general unifier of the two terms, iff they are unifiable.
    */
  def unify[T >: Variable](term1: T, term2: T): Option[Substitution[Variable, T]] = (term1, term2) match {
    // Dummy value
    case _ => Some(empty: Map[Variable, T])
  }


  /**
    * Collect all children, i.e. subterms, of the given term.
    *
    * This function does not only collect all immediate subterms,
    * but all children of a given term.
    */
  def children[T: ClassTag](term: T): List[T] = {
    import scala.collection.mutable.ListBuffer

    val childs = new ListBuffer[T]()

    // Collect all children
    val collect = query {
      case t: T => childs += t
    }
    
    // FIXME: Find a strategy which traverses only the imediate subterms
    breadthfirst(collect)(term)

    childs.toList
  }


  /**
    * The arity of a term, i.e. the number of all children of a given term.
    */
  def arity[T](term: T): Int = children[Any](term).length


  /**
    * Occurs check for two terms.
    */
  implicit class OccursCheck[S](s: S) {

    /**
      * Determine if 's' occurs at least one time in the term 't'.
      * 
      * @return True, iff 's' occurs in 't'.
      */
    def occursIn[T](t: T): Boolean = {

      // Strategy finding all elements which are equal to the term 's'.
      val equals = strategy {
	case term if term == s => Some(s)
      }

      // If the application of the strategy fails, 's' does not occur in 't'.
      oncebu(equals)(t).isDefined
    }
  }



  implicit class Substitution[A, B](substitution: Map[A, B]) {

  }

}

object Test extends Unification {

  abstract class Term
  case class Var(ide: String) extends Term
  case class Abs(ide: String, body: Term) extends Term
  case class App(f: Term, e: Term) extends Term
  case class Tuple(elems: List[Term]) extends Term
  case class MetaVar(ide: String) extends Term

  type Variable = MetaVar


  val testTerm = App(Abs("y", App(Var("y"), Var("y"))),
		     Var("x"))

  val testTuple = Tuple(List(Var("x"), Var("y"), Var("z")))
}
