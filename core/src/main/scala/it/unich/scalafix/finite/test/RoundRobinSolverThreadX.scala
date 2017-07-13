/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import it.unich.scalafix.FixpointSolverListener.EmptyListener
import it.unich.scalafix.{Assignment, FixpointSolverListener}
import scala.collection.mutable



/**
  * A fixpoint solver based on the round robin strategy.
  */
object RoundRobinSolverThreadX {
  /**
    * Solve a finite equation system.
    *
    * @tparam U type of the unknowns for the equation system
    * @tparam V type of values of the equatiom system
    * @param eqs      equation system to solve
    * @param start    a assignment to start the evaluation (defaults to `eqs.initial`)
    * @param listener a listener to track the behaviour of the solver (defaults to `EmptyListener`)
    * @return the solution of the equation system
    */
  //def apply[U, V](eqs: FiniteEquationSystem[U, V],nthread:Int, test:mutable.Map[U,V],listener: FixpointSolverListener[U, V] = EmptyListener)
  def apply[U, V](eqs: FiniteEquationSystem[U, V],nthread:Int,test:mutable.Map[U,V],listener: FixpointSolverListener[U, V] = EmptyListener)
                //(start: Assignment[U, V] = eqs.initial,
                 // listener: FixpointSolverListener[U, V] = EmptyListener
                //)
                : Assignment[U, V] = synchronized{
 /*   if(test==null){ // da controllare
    val test = mutable.HashMap.empty[U, V].withDefault(start) 
    }*/
    var lista:Iterable[U]= Nil
    nthread match {
    case 1 =>  lista = { listener.initialized(test); eqs.unknowns.zipWithIndex.filter { _._2 < eqs.unknowns.size / 2 }.map { _._1 }};
    case 2 => lista = { eqs.unknowns.zipWithIndex.filter { _._2 >= eqs.unknowns.size / 2 }.map { _._1 };}
    case _ => lista = eqs.unknowns
  } 
    val threadId = Thread.currentThread().getId();
  
   //listener.initialized(test)
 
     

   //for(j<-1 to eqs.unknowns.size){

    var dirty = true
    while (dirty) {    
      dirty = false
      /* if(nthread==2){
            Thread.`yield`()
        }*/
    //  this.synchronized{   
       
                 for (x <- lista) {
                  var newval = eqs.body(test)(x)                 
                    
              listener.evaluated(test, x, newval)    
              if (newval != test(x)) {
                          test(x) = newval
                              dirty = true
                              println("IO SONO "+threadId)
                         
                      }
                 //  }
      }
    //}
   }
    listener.completed(test)
    test
  }
 
}
