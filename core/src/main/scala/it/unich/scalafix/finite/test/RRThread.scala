package it.unich.scalafix.finite

import java.util.concurrent.Callable
import java.lang.Boolean
import scala.collection.mutable
import it.unich.scalafix.FixpointSolverListener
import it.unich.scalafix.FixpointSolverListener.EmptyListener

class RRThread[U,V](lista:Iterable[U], eqs: FiniteEquationSystem[U, V],var current: mutable.Map[U,V],listener: FixpointSolverListener[U, V] = EmptyListener) extends Callable[Boolean]{
  
  override def call():Boolean = {
   var dirtyThread=false
  // println(Thread.currentThread().getId+" "+lista)
   //println(" "+lista+" "+Thread.currentThread().getId+" "+System.currentTimeMillis())
   for (x <-  lista) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) {
          current(x) = newval
          dirtyThread = true
        }
      }
   dirtyThread
  }
}

