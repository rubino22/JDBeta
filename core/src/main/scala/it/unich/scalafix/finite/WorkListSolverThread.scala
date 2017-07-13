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
import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import scala.collection.mutable.LinkedHashSet
import scala.collection.JavaConversions._
import scala.concurrent.{Future}
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit




/**
  * A fixpoint solver based on a worklist.
  */
object WorkListSolverThread {
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
    val current = mutable.HashMap.empty[U, V].withDefault(start)
    listener.initialized(current)
    var workList = collection.mutable.LinkedHashSet.empty[U]
    var workListTemp = collection.mutable.LinkedHashSet.empty[U]
    val executor: ExecutorService = Executors.newWorkStealingPool()
    var listaThread=scala.collection.immutable.Set[Callable[LinkedHashSet[U]]]()
      val executor2: ExecutorService = Executors.newFixedThreadPool(1);
   workList ++= eqs.unknowns
   var dirty=true
  /*
   
     for(j<-0 until(testlist.size)){
  
 
 listaThread.+= (new Callable[LinkedHashSet[U]]() {
  def call():LinkedHashSet[U] = {
     var workListThread= collection.mutable.LinkedHashSet.empty[U]    
       workListThread++=testlist(j)     
     for(i<-0 until workListThread.size){
      val x = workListThread.head
      workListThread.remove(x)
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        println("C")
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workListThread ++= eqs.infl(x)
        
      }
     }
   return  workListThread
  }       
       })
 
    }
    
     
while ((!workList.isEmpty) || dirty==true){
 
  var resto= executor.invokeAll(listaThread)
        for(j<-0 until resto.size()){
        
      println(resto.get(j).get)
        }         
}*/
    
 //   FUNZIONA CON IL tempo di attesa

     for(j<-0 until(testlist.size)){
 listaThread.+= (new Callable[LinkedHashSet[U]]() {
  def call():LinkedHashSet[U]  = {
    var workListTemp = collection.mutable.LinkedHashSet.empty[U]
     workListTemp++= testlist(j)
      
 while (!workListTemp.isEmpty) {   
  val x = workListTemp.head    
      workListTemp.remove(x)
      val newval = eqs.body(current)(x)
    
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workListTemp ++= eqs.infl(x)
      }
    }
    
workListTemp
  }})
  
     }
    //  }
       
  var resto=executor.invokeAll(listaThread)
 for(i<-0 until resto.size){
  // println(resto.get(i).get)
 }
  

//executor.awaitTermination(1, TimeUnit.MILLISECONDS)
       
  
   /* RISULTATI DIVERSI
    for(j<-0 until testlist.size){
      var temp=testlist(j)
     
         listaThread.+=(new  Callable[Boolean]() {
  def call(): Boolean = {
    var dirty=false  
    
     var x = workList.head
     for(i<-0 until temp.size){ 
      if(workList.contains(temp.toList.get(i)))
      {x=temp.toList.get(i)
        
      }
     }
    
      workList.remove(x)
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workList ++= eqs.infl(x)
      }
  return workList.isEmpty
}});       
       }
    while(dirty){ 
        
          dirty=false
        var resto= executor.invokeAll(listaThread)
        while(executor.awaitTermination(1, TimeUnit.SECONDS))
        for(j<-0 until resto.size()){
          if(resto.get(j).get){
           println("J "+j +" "+resto.get(j).get)
            dirty=true
          }
        }
       }
       * 
       */

/* FUnzionante in parte
   for(j<-0 until testlist.size){
          var temp=testlist(j)
val future = new FutureTask[Boolean](new Callable[Boolean]() {
  def call(): Boolean = {
     var workListTemp = testlist(j)
     while (!workListTemp.isEmpty) {
     //workList.head
    //for(i<-0 until temp.size){ 
      
     var x =workListTemp.head
        
     
    
      workListTemp.remove(x)
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workListTemp ++= eqs.infl(x)
      
     }
  }
    // }    
   //  println("-->"+workList.isEmpty)
  return workListTemp.isEmpty
}} )
executor.execute(future)
 
 // val blockingResult = Await.result(future, Duration.Inf)
   }
 */


//var workListTemp= collection.mutable.LinkedHashSet.empty[U]
 /*
  executor.submit(new Callable[LinkedHashSet[U]]() {
  def call():LinkedHashSet[U] = {     
    println("worklist Thread "+workList)
   while (!workList.isEmpty) {
     println("XXX")
      val x = workList.head
      workList.remove(x)
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workList ++= eqs.infl(x)
      }
    }
   return  workList
  }
       
       })
     
    */ 
     
 

  
 
  
  /* while (!dirty) { 
     
        var resto= executor.invokeAll(listaThread)
       
        for(j<-0 until resto.size()){
           println("resto"+resto.get(j).get)
       dirty=resto.get(j).get
            
        }
     println("USCITO")
       }
 */
    
  
    
 
    // is it better to use a Queue for a worklist ?
  
  /*
    workList ++= eqs.unknowns
    while (!workList.isEmpty) {
      val x = workList.head
      workList.remove(x)
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workList ++= eqs.infl(x)
      }
    }
    */
    executor.shutdownNow()
    listener.completed(current)
    current
   
  }
}
