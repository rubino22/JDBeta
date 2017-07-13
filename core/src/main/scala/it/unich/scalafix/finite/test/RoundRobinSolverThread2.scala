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
import java.util.Arrays.ArrayList
import java.util.concurrent.Future
import java.util.concurrent.ThreadPoolExecutor
import java.util.Collection
import java.util.Arrays.ArrayList
import java.util.concurrent.FutureTask
import scala.concurrent.Await
import java.util.concurrent.Callable
import java.util.{List => JList, ArrayList}
import scala.collection.JavaConversions._
import java.util.concurrent.CompletionService
import scala.concurrent._
import java.util.concurrent.ForkJoinPool
import scala.collection.Parallel




/**
  * A fixpoint solver based on the round robin strategy.
  */
object RoundRobinSolverThread2  extends App {
 
    
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
         
       val executor: ExecutorService = Executors.newFixedThreadPool(4)
       
      val fjpool = new ForkJoinPool(4)
    
      // val pool: ExecutorService = Executors.newFixedThreadPool(4)
       //val finale = eqs.unknowns.zipWithIndex.filter { _._2 >= eqs.unknowns.size / 2 }.map { _._1 }
      // val iniziale = eqs.unknowns.zipWithIndex.filter { _._2 < eqs.unknowns.size / 2 }.map { _._1 }
      //LISTA DINAMICA
       //val nProcessors = Runtime.getRuntime.availableProcessors
       //var split=(if(eqs.unknowns.size > nProcessors.toInt-1)  nProcessors.toInt-1 else eqs.unknowns.size)
     
       
         //val mio:List[Callable[Boolean]]=null
   /*
  for(j<-0 until testlist.size){ 
   var listas= testlist(j) 
  
  var t= executor.submit(new Callable[Boolean]() {
  def call(): Boolean = {
    var dirtiThread=false
    for (x <-  listas) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          dirtiThread = true
        }
      }
   dirtiThread
}})
  }    
  */     
      // println("qui")
       var dirty=true
       //val testlist= eqs.unknowns.sliding(Math.round(eqs.unknowns.size.toFloat / split),Math.round(eqs.unknowns.size.toFloat / split)).toList   
       var listaThread=List[RRThread[U,V]]()
     
   /*QUI INIZIA UN TEST
      
       
      while(dirty){
         var listarisultai=List[Boolean]()
        dirty=false
        var t=false
         for(j<-0 until testlist.size ){ 
   var listas= testlist(j) 
  
   t = executor.submit(new Callable[Boolean]() {
  def call(): Boolean = {
    var dirtiThread=false
    for (x <-  listas) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          dirtiThread = true
        }
      }
   dirtiThread
}}).get
listarisultai = listarisultai :+t
 
         }    
  for(i<- 0 until listarisultai.size if(dirty!= true)){
     if(listarisultai(i)){
       dirty=true
     }
   }
      
    }   
     //QUI FINISCE UN TEST  */
     
       /* ORIGINALE  togliere qui*/
       //Carico i thread all'interno di una lista */
         for(j<-0 until testlist.size){   
         listaThread = listaThread :+ new  RRThread(testlist(j),eqs,current,listener)        
       }
 
         while (dirty) {    
      dirty=false    
      
       var test=executor.invokeAll(listaThread) /* eseguo i thread insieme */
  
         
       for(i<-0 until test.size(); if(dirty != true)){
         if(test.get(i).get){
           dirty=true        
         }
       }
        
       //  executor.awaitTermination(Long.MinValue, TimeUnit.NANOSECONDS);
    }
         /*FINE ORIGNALE TOGLIRE QUI
       */   
  //  FINE LISTA DINAMICA
  
       
    //LISTA STATICA    
    /* 
      val finale = eqs.unknowns.zipWithIndex.filter { _._2 >= eqs.unknowns.size / 2 }.map { _._1 }
      val iniziale = eqs.unknowns.zipWithIndex.filter { _._2 < eqs.unknowns.size / 2 }.map { _._1 }
      var t0 = new RRThread(iniziale,eqs,current,listener)      
      var t1 = new RRThread(finale,eqs,current,listener)
      var lista=List[RRThread[U,V]]()
      lista = lista :+ t0
      lista = lista :+ t1
     
      var test=executor.invokeAll(lista)    
      
    while ( test.get(0).get || test.get(1).get) {           
         test=executor.invokeAll(lista)      
       }
        //FINE LISTA STATICA
    */
     
      executor.shutdown()
    listener.completed(current)
    current
  }  
}


