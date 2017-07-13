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
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.Collection
import java.util.concurrent.Callable
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.collection.Parallel
import scala.collection.immutable.Set







/**
  * A fixpoint solver based on the round robin strategy.
  */
object RoundRobinSolverThread  extends App {
 
    
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
  def apply[U, V](eqs: FiniteEquationSystem[U, V],testlist:List[Iterable[U]])
                 (start: Assignment[U, V] = eqs.initial,
                  listener: FixpointSolverListener[U, V] = EmptyListener): Assignment[U, V] = {
    var current = mutable.HashMap.empty[U, V].withDefault(start)

    listener.initialized(current)            
    val executor: ExecutorService = Executors.newWorkStealingPool()
    var dirty=true
    var listaThread=scala.collection.immutable.Set[Callable[Boolean]]()
   
     /*
      * INIZIO NORMALE
      
    // var i=0
        while (dirty) {    
      dirty=false    
     // println("I "+i+" "+current)
     // i=i+1;
       for(j<-0 until testlist.size){
         
 if( executor.submit(new Callable[Boolean]() {
  def call(): Boolean = {
    var dirtiThread=false
       for (x <-  testlist(j) ) {   
       //  println("X "+x)
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          dirtiThread = true
         //println("TRUE "+x)
        }
      }
   dirtiThread
}}).get == true ){
  dirty=true
}
         
        
    /*    executor.execute(new Runnable() {
  def run()  = {
  // println(Thread.currentThread().getId+" "+testlist(j))
    //var dirtiThread=false
    for (x <-  testlist(j)) {
      println("XXX "+j)
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) { 
          current(x) = newval
          dirty= true
          println(true+" "+testlist(j)+" "+x)
        }
      }
   
}})*/
    
        }
       }
     
      * FINE NORMALE
      */
     
    /*
     * Con Lista
    */
    //cerchiamo di creare lista immutable
    for(j<-0 until testlist.size){   
         listaThread.+=(new  Callable[Boolean]() {
  def call(): Boolean = {
    var dirty=false     
    for (x <-  testlist(j)) {      
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          dirty = true        
        }
      }
  return dirty
}});       
       }

       while(dirty){ 
        
          dirty=false
        var resto= executor.invokeAll(listaThread)
        for(j<-0 until resto.size()){
          if(resto.get(j).get)
            dirty=true
        }
       }
        /*
         * Fine con Lista  
         */
          
         
     
      executor.shutdownNow()
    listener.completed(current)
    current
  }  
}


