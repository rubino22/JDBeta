package it.unich.scalafix.finite

import java.util.concurrent.Callable
import java.lang.Boolean
import scala.collection.mutable
import it.unich.scalafix.FixpointSolverListener
import it.unich.scalafix.FixpointSolverListener.EmptyListener

class RRThreadRun[U,V](var dirty:Boolean,lista:Iterable[U], eqs: FiniteEquationSystem[U, V],current: mutable.Map[U,V],listener: FixpointSolverListener[U, V] = EmptyListener)extends Runnable{

  
  
  /*class Provami(dirty1:Boolean,lista1:Iterable[U], eqs1: FiniteEquationSystem[U, V],current1: mutable.Map[U,V],listener1: FixpointSolverListener[U, V] = EmptyListener){
    dirty=dirty1
    eqs=eqs1
    current=current1
    listener=listener1
  }*/
  
  def run() = {
   println(Thread.currentThread().getId+" "+lista)
   //println(" "+lista+" "+Thread.currentThread().getId+" "+System.currentTimeMillis())
   for (x <-  lista) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          println("TRUE "+lista)
          dirty= true
        }
      }
   
  }
}

  
