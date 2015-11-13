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

import java.io.{ File, FileReader }
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
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
import it.unich.jandom.domains.numerical.ParallelotopeDomainModRationalGmpExt
import it.unich.jandom.utils.numberext.ModRationalGmpExt
import parma_polyhedra_library.PPL_Object
import it.unich.jandom.domains.numerical.ppl.PPLDomainSuiteOctagon
import parma_polyhedra_library.Octagonal_Shape_double
import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.domains.numerical.ParallelotopeDomainModQSpire
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.ProductDomainSuite
import it.unich.jandom.domains.numerical.BoxSpireDomain
import parma_polyhedra_library.Double_Box
import scala.collection.mutable.ArrayBuffer



/**
 * Example program using ''Jandom'' to analyze the Alice benchmarks and
 * compare the results with different parameters. In this moment, it compares
 * the result of the analyisis with standard Kleene iteration and worklist
 * based ones.
 */
object JandomModQBench extends App {

  var totalEquals = 0
  var totalBestPPL = 0
  var totalBestOther = 0
  var totalUncomparable = 0
  var totalPrograms = 0
  var totalTimeGMP =0.0
  var totalTimeSpire =0.0
  def CStoPolyehdra(dimension: Int, c: Seq[LinearForm[Double]]) = {
    //println("Trasfomrazione");
    val d = PPLDomain[C_Polyhedron]    
    //println("c "+c+" "+d.top(dimension));
    c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm[Double]) => p.linearInequality(lf) }
  }

  def mkString[U <: DimensionFiberedProperty[U]](program: LTS, m: scala.collection.Map[LTS#ProgramPoint, U]): String = {
    (for ((loc, prop) <- m) yield loc.name + " => " + prop.mkString(program.env.variables)).mkString(", ")
  }
 val Q = ArrayBuffer[String]()
  def fastModelAnalyze(model: File) = {
    totalPrograms += 1

    println(s"------>${model}")

    val fr = new FileReader(model)
    val source = new PagedSeqReader(PagedSeq.fromReader(fr))
    val parsed = FastParser().parse(source)
    fr.close()
    val program = parsed.get
    println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
/*
    val params1 = new targets.Parameters[LTS] { val domain = PPLDomain[Octagonal_Shape_double]()}
    params1.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopes
    params1.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3) // needed for parallelotopes
    //params3.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params1) // warmup JVM
    //params3.debugWriter.flush()

    val t1 = System.currentTimeMillis
    val ann1 = program.analyze(params1)
    val tann1 = System.currentTimeMillis - t1
    
  
    val params2 = new targets.Parameters[LTS] { val domain = PPLDomain[C_Polyhedron]()}
    params2.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopes
    params2.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params3.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params2) // warmup JVM
    //params3.debugWriter.flush()

    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params2)
    val tann2 = System.currentTimeMillis - t2
    

/*
    val params3 = new targets.Parameters[LTS] { val domain = ParallelotopeDomain() }
    params3.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopes
    params3.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params3.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params3) // warmup JVM
    //params3.debugWriter.flush()

    val t3 = System.currentTimeMillis
    val ann3 = program.analyze(params3)
    val tann3 = System.currentTimeMillis - t3
*/
    
    val params4 = new targets.Parameters[LTS] { val domain = ParallelotopeDomainModQSpire(1,overRound=false) }
    params4.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
    params4.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params4.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params4) // warmup JVM
    //params4.debugWriter.flush()

    val t4 = System.currentTimeMillis
    val ann4 = program.analyze(params4)
    val tann4 = System.currentTimeMillis - t4
    */
    val params5 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxSpireDomain(),ParallelotopeDomainModQSpire(-1,overRound=false)) }
    params5.wideningFactory = DelayedWideningFactory(DefaultWidening, 1) // needed for parallelotopesModQExt
    params5.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 1)
    //params5.debugWriter = new java.io.PrintWriter(System.out)

   // program.analyze(params5) // warmup JVM
   //params5.debugWriter.flush()

    val t5 = System.currentTimeMillis
    val ProDom = program.analyze(params5)
    val tann5 = System.currentTimeMillis - t5
   
  println("box");
    val params6 = new targets.Parameters[LTS] { val domain =  BoxSpireDomain()}
    params6.wideningFactory = DelayedWideningFactory(DefaultWidening, 1) // needed for parallelotopesModQExt
    params6.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 1)
   // params6.debugWriter = new java.io.PrintWriter(System.out)
   // params6.debugWriter
   // program.analyze(params6) // warmup JVM
    //params6.debugWriter.flush()

    val t6 = System.currentTimeMillis
    val Box = program.analyze(params6)
    val tann6 = System.currentTimeMillis - t6
    println("ptope");
     val params6Bis = new targets.Parameters[LTS] { val domain = ParallelotopeDomainModQSpire(1,overRound=false) }
    params6Bis.wideningFactory = DelayedWideningFactory(DefaultWidening, 1) // needed for parallelotopesModQExt
    params6Bis.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 1)
    //params6Bis.debugWriter = new java.io.PrintWriter(System.out)

   // program.analyze(params6Bis) // warmup JVM
   //params6Bis.debugWriter.flush()

    val t6Bis = System.currentTimeMillis
    val par = program.analyze(params6Bis)
    val tann6Bis = System.currentTimeMillis - t6Bis
   /*
    val params7 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxDoubleDomain(),ParallelotopeDomainModQSpire(-1,overRound=false)) }
    params7.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
    params7.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params4.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params7) // warmup JVM
    //params4.debugWriter.flush()

    val t7 = System.currentTimeMillis
    val ann7 = program.analyze(params7)
    val tann7 = System.currentTimeMillis - t7
    
    
    val cann1 = ann1 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann2 = ann2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   // val cann3 = ann3 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   // val cann4 = ann4 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann5 = ann5 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann6 = ann6 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   // val cann6Bis = ann6Bis mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann7 = ann7 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }*/
println("INIZIO")
    println(ProDom);
    println("---------------------------------------")
    println(Box);
    println("---------------------------------------")
    println(par);   
    println("FINE")
    ;

    val cprdoDom = ProDom mapValues { p => CStoPolyehdra(p.p1.dimension, p.p1.constraints) intersection(CStoPolyehdra(p.p2.dimension, p.p2.constraints)) } 
    val cprdoDom1 = ProDom mapValues { p => (CStoPolyehdra(p.p1.dimension, p.p1.constraints))}
    val cprdoDom2 = ProDom mapValues { p => (CStoPolyehdra(p.p2.dimension, p.p2.constraints))}
    //val cprdoDom2=  cprdoDom  map {case (loc, v) => (loc ->(cprdoDom(loc) intersection(cprdoDom1(loc))))}
    println(cprdoDom)
    println(cprdoDom1)
    println(cprdoDom2)
    val cbox = Box mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
     val cpar = par mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   //totalTimeGMP=totalTimeGMP + tann4;
   //totalTimeSpire=totalTimeSpire + tann6;
   // println(s"Times:  ${tann4} vs  ${tann6}")
   /* print("PPL Octagon_Shape_double: ")
    println(mkString(program, cann1))
    print("PPL C_Polyhedron: ")
    println(mkString(program, cann2)) */
   /* print("PTope: ")
    println(mkString(program, cann3))
    print("PTopeExt: ")
    println(mkString(program, cann4))*/
    print("Box: ")
    println(mkString(program, cbox))
    print("Par: ")
    println(mkString(program, cpar))
    print("ProdDomain: ")
    println(mkString(program, cprdoDom))
   

    // SOSTITUIRE cann1 con cann3 se si vuole il confronto con i Parallelotopi.
    // val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann1(loc) intersection cann5(loc))) }

    //comparing sum with box
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann1(loc))) }

    //comparing sum with parallelotope
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann3(loc))) }
    
    //comparing sum with parallelotope
  //  val comp = cprdoDom map { case (loc, v) => (loc ->{println(cprdoDom);println(cbox(loc) intersection(cpar(loc))); v.tryCompareTo(cbox(loc) intersection(cpar(loc)))}) }
    val comp = cprdoDom  map { case (loc, v) => (loc ->v.tryCompareTo(cbox(loc) intersection(cpar(loc)))) }

    println("COUNT EQUALS: " + comp.count(_._2 == Some(0)))
    println("COUNT BETTER Prod: " + comp.count(_._2 == Some(-1)))
    println("COUNT BETTER Int: " + comp.count(_._2 == Some(1)))
    println("COUNT UNCOMPARABLES: " + comp.count(_._2 == None))
//if(comp.count(_._2 == Some(1))>0) {Q += s"------>${model}"}
    totalEquals += comp.count(_._2 == Some(0))
    totalBestPPL += comp.count(_._2 == Some(-1))
    totalBestOther += comp.count(_._2 == Some(1))
    totalUncomparable += comp.count(_._2 == None)

    //println("DIFFERENT BEHAVIOURS: " + model)
    //println(s"Times:  ${tann1} vs  ${tann2}")
    //println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
 
    println("EQUALS: " + program.locations.filter(comp(_) == Some(0)).map(_.name).mkString(", "))
    println("BETTER PRO: " + program.locations.filter(comp(_) == Some(-1)).map(_.name).mkString(", "))
    println("BETTER INT: " + program.locations.filter(comp(_) == Some(1)).map(_.name).mkString(", "))
    println("UNCOMPARABLES: " + program.locations.filter(comp(_) == None).map(_.name).mkString(", "))
  }

val singolo=true;
var badPrograms = Seq[File]()
if(singolo ==false){
  val resources = getClass.getResource("/fast/").toURI;
  for (model <- new File(resources).listFiles()) {
    try {
      fastModelAnalyze(model)
    } catch {
      case e: IllegalArgumentException =>
        badPrograms +:= model
    }
  }
}
else{//aaron2
 val resources2 = getClass.getResource("/fast/metro.fst").toURI;
  val file= new File(resources2)  
   fastModelAnalyze(file) 
}
 
  
 //println("FINALE "+Q);

  

  // This analyzes all models (does not terminate for descending2 with

 

  /*
*/
  // This is if we want to analyze a specificic model
  // fastModelAnalyze(new File(resources.resolve("terminate.fst")))

  println("\nFinal results:")
  println("---------------")
  println(s"Number of programs: ${totalPrograms}")
  println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
  println(s"Total equals: ${totalEquals}")
  println(s"Total best prod: ${totalBestPPL}")
  println(s"Total best int: ${totalBestOther}")
  println(s"Total uncomparables: ${totalUncomparable}")
 /* print(s"Total time GMP: ${totalTimeGMP}  ")
  print(s"  vs    Total time Spire: ${totalTimeSpire}")
*/
}
