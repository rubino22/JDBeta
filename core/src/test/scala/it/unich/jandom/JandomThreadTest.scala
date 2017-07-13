/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom

import it.unich.scalafix.finite._
import java.io.{ File, FileReader }

import it.unich.jandom.domains.DimensionFiberedProperty
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.ParallelotopeDomain
import it.unich.jandom.domains.numerical.SumIntParallelotopeDomain
import it.unich.jandom.domains.numerical.ppl.PPLDomain
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.ppfactories._
import it.unich.jandom.ppfactories.PPFactory.ConstantFactory
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.widenings.DefaultWidening
import parma_polyhedra_library.C_Polyhedron
import parma_polyhedra_library.PPL_Object
import it.unich.jandom.domains.numerical.ppl.PPLDomainSuiteOctagon
import parma_polyhedra_library.Octagonal_Shape_double
import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.domains.numerical.ParallelotopeRationalDomain
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.ProductDomainSuite
import it.unich.jandom.domains.numerical.BoxRationalDomain
import parma_polyhedra_library.Double_Box
import scala.collection.mutable.ArrayBuffer
import it.unich.jandom.domains.numerical.ppl.PPLProperty
import java.util.Calendar
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.FixpointSolver._
import it.unich.jandom.targets.lts.Location
import java.util.concurrent.Executors
import java.util.concurrent._
import it.unich.scalafix.Body
import it.unich.scalafix.Assignment
import scala.collection.mutable
import java.util.concurrent.Callable
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.impl.Future





/**
 * Example program using ''Jandom'' to analyze the Alice benchmarks and
 * compare the results with different parameters. In this moment, it compares
 * the result of the analyisis with standard Kleene iteration and worklist
 * based ones.
 */
object ProvaThread extends App {

  var totalEquals = 0
  var totalBestPPL = 0
  var totalBestOther = 0
  var totalUncomparable = 0
  var totalPrograms = 0
  var totalTimeThread = 0.0
  var totalTimeOriginale = 0.0
  var totalTimeOriginaleLTS = 0.0
 var count=0

  def dropIndex[T](xs: List[T], n: Int) = {
    val (l1, l2) = xs splitAt n
    l1 ::: (l2 drop 1)
  }

  def extractIndex[T](xs: List[T], n: Int): (T, List[T]) =
    (xs(n), dropIndex(xs, n))

  def mkString[U <: DimensionFiberedProperty[U]](program: LTS, m: scala.collection.Map[LTS#ProgramPoint, U]): String = {
    (for ((loc, prop) <- m) yield loc.name + " => " + prop.mkString(program.env.variables)).mkString(", ")
  }
  val Q = ArrayBuffer[String]()
  def fastModelAnalyze(model: File) = {
    totalPrograms += 1

    println(s"------>${model}")
    println(s"Time Start ${Calendar.getInstance().getTime()}")
   
    val source = new FileReader(model)
    val parsed = FastParser().parse(source)
    source.close()
    val program = parsed.get
    
    println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
    val tes = program.env.variables
    val delay = (tes.length * 3)
    val dom =ParallelotopeRationalDomain(1)

    
    /**
     * test ScalaFix
     */
    implicit val scalafixDomain = dom.ScalaFixDomain
    val wideningBox = { (x: dom.Property, y: dom.Property) => x widening y }
    val narrowingBox = { (x: dom.Property, y: dom.Property) => x narrowing y }
    val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.WorkListSolver, wideningBox, narrowingBox)
    val CC77Thread = FiniteFixpointSolverThread.CC77[Location, dom.Property](Solver.WorkListSolverThread, wideningBox, narrowingBox)
       
    val ges=program.toEQS(dom)
    
    val ges2= program.toDot
    
    
        
    
    
     val nProcessors = Runtime.getRuntime.availableProcessors
       /* split è ottimizzato per il massimo */
     
       var split=(if(ges.unknowns.size > nProcessors.toInt-1)  nProcessors.toInt-1 else ges.unknowns.size)
        
       val testlist= ges.unknowns.sliding(Math.round(ges.unknowns.size.toFloat / split),Math.round(ges.unknowns.size.toFloat / split)).toList
    
       
       
    // FiniteFixpointSolver(ges, CC77Thread)
    val t3 = System.currentTimeMillis
    val t=FiniteFixpointSolverThread(ges, CC77Thread,testlist)
    val thread = System.currentTimeMillis - t3
    println("THREAD"+thread)
    
    
    
    /*LTS
    val params2 = new targets.Parameters[LTS] { val domain = ParallelotopeRationalDomain(0) } 
    
    params2.wideningFactory = DelayedWideningFactory(DefaultWidening, delay) // needed for parallelotopes
    params2.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, delay)   
    program.analyze(params2) // warmup JVM
    params2.debugWriter.flush()
  
    val ann7 = program.analyze(params2)
    
    
    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params2)
    val tradizione = System.currentTimeMillis - t2
   // println("FIne LTS")
     FINE LTS */
    
    /* Senza Thread  */
    // FiniteFixpointSolver(ges, CC77)
   println("----------------------------")
    val t4 = System.currentTimeMillis
    val tOriginale=FiniteFixpointSolver(ges, CC77)      
    val originale = System.currentTimeMillis - t4
    
    println("Scalafix "+originale)
   /* Fine senza Thread */
    
    /* Calcola il numero di core */
      
    /* FINE THREAD */
    
   //println("Scalafix")
   // for (x <- ges.unknowns) print(x+" -> "+tOriginale(x))
   //// println()
     //println("Scalafix Thread")
     //for (x <- ges.unknowns) print(x+" -> "+t(x));
    
    for (x <- ges.unknowns) if(!t(x).equals(tOriginale(x))){   //count=count+1 
     println("Diversi Thread= "+t(x));
     println( "ScalaFix"+tOriginale(x));
    }
    println()
  //  println("LTS")
    //println("Thread    "+t+" Time "+thread)
    //println(ann2)

    totalTimeThread=totalTimeThread + thread;     
    totalTimeOriginale=totalTimeOriginale + originale;       
    //totalTimeOriginaleLTS=totalTimeOriginaleLTS + tradizione;      

  
  }

  val singolo = true
  var badPrograms = Seq[File]()
  if (singolo == false) {
    val resources = getClass.getResource("/fast/").toURI;
    for (model <- new File(resources).listFiles()) {
      try {
       // parallelotopi zero if(!model.getName.equals("exmini.fst") &&  !model.getName.equals("terminate.fst") &&  !model.getName.equals("aaron2.fst") && !model.getName.equals("perfect.fst") )
        //if(!model.getName.equals("berkeley.fst") && !model.getName.equals("halbwachs4.fst")) //Ptope(-1) delay n*3 // 2 to 
        //   if(!model.getName.equals("multcounters3.fst") && !model.getName.equals("interleaving2.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("interleaving1.fst") && !model.getName.equals("gulwani2_alt.fst")) //C_Polyhedra  //
        // if(!model.getName.equals("multcounters3.fst")  && !model.getName.equals("interleaving2.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("interleaving1.fst") && !model.getName.equals("gulwani2_alt.fst") && !model.getName.equals("perfect.fst") && !model.getName.equals("gulwani2.fst") )

        fastModelAnalyze(model)
      } catch {
        case e: IllegalArgumentException =>
          badPrograms +:= model
      }
    }
  } else { //aaron2
    val resources2 = getClass.getResource("/fast/ackerman.fst").toURI;
    val file = new File(resources2)
    fastModelAnalyze(file)
  }
  println("TOTAL TIME SCALAFIX Thread "+totalTimeThread);
  println("TOTAL TIME SCALAFIX "+totalTimeOriginale);
  println("TOTAL TIME LTS "+totalTimeOriginaleLTS);
  println("COunt "+count)

}

