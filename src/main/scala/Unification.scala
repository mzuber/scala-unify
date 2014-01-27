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
  def unify[T](term1: T, term2: T): Option[Substitution[Variable, T]] = {
    None
  }


  def children[T: ClassTag](term: T): List[T] = {
    // Collect all children
    val childs = collectl {
      case x => x
    }(term)

    // Filter subterms, i.e., children which have type T
    childs.tail collect {
      case t: T => t
    }
  }


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

}

object Test extends Unification {

  abstract class Term
  case class Var(ide: String) extends Term
  case class Abs(ide: String, body: Term) extends Term
  case class App(f: Term, e: Term) extends Term


  val testTerm = App(Abs("x", Var("x")),
		     Var("x"))
}
