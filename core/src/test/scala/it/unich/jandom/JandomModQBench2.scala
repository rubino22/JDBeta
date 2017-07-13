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
import parma_polyhedra_library.Rational_Box
import it.unich.jandom.domains.numerical.ParallelotopeRationalDomainFast
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningSpecs._

import it.unich.jandom.targets.parameters.IterationStrategy

import it.unich.jandom.domains.numerical.ppl.PPLDomainMacro



/**
 * Example program using ''Jandom'' to analyze the Alice benchmarks and
 * compare the results with different parameters. In this moment, it compares
 * the result of the analyisis with standard Kleene iteration and worklist
 * based ones.
 */

/*
object Prova extends App {

  var totalEquals = 0
  var totalBestPPL = 0
  var totalBestOther = 0
  var totalUncomparable = 0
  var totalPrograms = 0
  var totalTimeAltro =0.0
  var totalTimeProd =0.0
  var mappaVariabili: scala.collection.immutable.Map[String,Int]=Map()
  var mappaStati: scala.collection.immutable.Map[String,Int]=Map()
  def CStoPolyehdra(dimension: Int, c: Seq[LinearForm]) = {
    //println("Trasfomrazione");
    val d = PPLDomain[C_Polyhedron]    
    //println("c "+c+" "+d.top(dimension));
    c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm) => {p.linearInequality(lf)} }
  }

  /*  def CStoBox(dimension: Int, c: Seq[LinearForm[Double]]) = {    
    val d = BoxSpireDomain()    
    c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm[Double]) => p.linearInequality(lf) }
  }*/
  
  def CStoBox(dimension: Int, c: Seq[LinearForm]) = {
    val d = PPLDomain[Double_Box]
    val poly = CStoPolyehdra(dimension, c)
    d(poly)
  }
    
  /* test
  def CStoOc(dimension: Int, c: Seq[LinearForm[Double]]) = {
    val d = PPLDomain[Octagonal_Shape_double]
    val poly = CStoPolyehdra(dimension, c)
    d(poly)
  }
  
  def CStoP(dimension: Int, c: Seq[LinearForm[Double]]) = {
    val d = ParallelotopeDomainModQSpire(-1,overRound=false)
    val poly = CStoPolyehdra(dimension, c)
    d(poly)
  }
         
    */
    
    
  
  def mkString[U <: DimensionFiberedProperty[U]](program: LTS, m: scala.collection.Map[LTS#ProgramPoint, U]): String = {
    (for ((loc, prop) <- m) yield loc.name + " => " + prop.mkString(program.env.variables)).mkString(", ")
  }
 val Q = ArrayBuffer[String]()
  def fastModelAnalyze(model: File) = {
    totalPrograms += 1

    println(s"------>${model}")
    println(s"Time Start ${Calendar.getInstance().getTime()}")
    val fr = new FileReader(model)
    val source = new PagedSeqReader(PagedSeq.fromReader(fr))
    val parsed = FastParser().parse(source)
    fr.close()
    val program = parsed.get
    println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
    val tes= program.env.variables
    
    //val delay= (tes.length *3)
   // println(delay)
      val delay=7
      val nome= model.toString().split("/").map(_.trim).toList.last
      mappaVariabili+=(nome->tes.length)
      mappaStati +=(nome -> program.numlocs)
    // println( +" "+tes.length+" "+program.numlocs)
 /*  
   //println("var "+delay+" "+program.locations.size);
      println("ptope normali");
    val params1 = new targets.Parameters[LTS] { val domain =  ParallelotopeDomain()}
    params1.wideningFactory = DelayedWideningFactory(DefaultWidening, delay) // needed for parallelotopes
    params1.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, delay) // needed for parallelotopes
    //params1.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params1) // warmup JVM
    //params1.debugWriter.flush()

    val t1 = System.currentTimeMillis
    val ann1 = program.analyze(params1)
    val tann1 = System.currentTimeMillis - t1
    println("Fine ptope normali e inizio dei trasformati");
*/    
  // val params2 = new targets.Parameters[LTS] { val domain = BoxRationalDomain() }
       
    // val params2 = new targets.Parameters[LTS] { val domain = PPLDomain[Octagonal_Shape_double]()}
       val params2 = new targets.Parameters[LTS] { val domain = PPLDomainMacro[C_Polyhedron] }
//      val params2 =  new targets.Parameters[LTS] { val domain = ParallelotopeDomain() }
      params2.widening = DefaultWidening
    params2.narrowing = DefaultNarrowing
    params2.iterationStrategy = IterationStrategy.Worklist
    //params2.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params2) // warmup JVM
    //params2.debugWriter.flush()

    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params2)
    val tann2 = System.currentTimeMillis - t2
    
 //println("--->"+ann2+" "+tann2);
/*
    val params3 = new targets.Parameters[LTS] { val domain = ParallelotopeDomainModQSpire(-1,overRound=false) }
    params3.wideningFactory = DelayedWideningFactory(DefaultWidening, delay) // needed for parallelotopes
    params3.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, delay)
    //params3.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params3) // warmup JVM
    //params3.debugWriter.flush()

    val t3 = System.currentTimeMillis
    val ann3 = program.analyze(params3)
    val tann3 = System.currentTimeMillis - t3

    
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
   val params5 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxRationalDomain(),ParallelotopeRationalDomainFast(-1)) }
 
  //  val params5 = new targets.Parameters[LTS] { val domain = ParallelotopeRationalDomainFast(1) }
    params5.wideningFactory = DelayedWideningFactory(DefaultWidening, delay) // needed for parallelotopesModQExt
    params5.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, delay)
   //params5.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params5) // warmup JVM
    //params5.debugWriter.flush()

    val t5 = System.currentTimeMillis
    val ProDom = program.analyze(params5)
    val tann5 = System.currentTimeMillis - t5
 
   /*println("box");
    val params6 = new targets.Parameters[LTS] { val domain =  BoxRationalDomain()}
    params6.wideningFactory = DelayedWideningFactory(DefaultWidening, delay) // needed for parallelotopesModQExt
    params6.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, delay)
   //params6.debugWriter = new java.io.PrintWriter(System.out)
    //params6.debugWriter
    program.analyze(params6) // warmup JVM
    //params6.debugWriter.flush()

    val t6 = System.currentTimeMillis
    val Box = program.analyze(params6)
    val tann6 = System.currentTimeMillis - t6
     */
  /*  
 //   println("ptope");
    /*  val params6Bis = new targets.Parameters[LTS] { val domain = ParallelotopeDomainModQSpire(1,overRound=false) }
    params6Bis.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
    params6Bis.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params6Bis.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params6Bis) // warmup JVM
   //params6Bis.debugWriter.flush()

    val t6Bis = System.currentTimeMillis
    val par = program.analyze(params6Bis)
    val tann6Bis = System.currentTimeMillis - t6Bis
  
    val params7 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxDoubleDomain(),ParallelotopeDomainModQSpire(-1,overRound=false)) }
    params7.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
    params7.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    //params4.debugWriter = new java.io.PrintWriter(System.out)

    program.analyze(params7) // warmup JVM
    //params4.debugWriter.flush()

    val t7 = System.currentTimeMillis
    val ann7 = program.analyze(params7)
    val tann7 = System.currentTimeMillis - t7
   */ 
   // println("OCtagon -->"+ann1)
   // println("ProDom --> "+Box);
   confronto su vincoli degli assi
   
      val altro = ann2 mapValues { p => CStoBox(p.dimension, p.constraints) } //octagon
      val cBox = Box mapValues { p => CStoBox(p.dimension, p.constraints) } //octagon
      val cprod = ProDom mapValues { p => CStoBox(p.dimension, p.constraints) } //ptope
    */
 // println("Can2 original "+ann1);
 // println("Can2 "+cann2);
   

    //confronto su tutti i vincoli   
 
   val altro = ann2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   //val cBox = Box mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   val cprod = ProDom mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    
   /*
     val altro = ann2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cBox = Box mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cprod = ProDom mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   
    */
 /*   val cann3 = ann3 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   // val cann4 = ann4 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann5 = ann5 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann6 = ann6 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   // val cann6Bis = ann6Bis mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann7 = ann7 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }*/
    /*println("INIZIO")
    println(ProDom);
    println("---------------------------------------")
    println(Box);
    println("---------------------------------------")
    println(par);   
    println("FINE")
    ;
*/
    //val cprdoDom = ProDom mapValues { p => CStoPolyehdra(p.dimension, p.constraints) } 
    //val cprdoDom1 = ProDom mapValues { p => (CStoPolyehdra(p.p1.dimension, p.p1.constraints))}Chiudi
    //val cprdoDom2 = ProDom mapValues { p => (CStoPolyehdra(p.p2.dimension, p.p2.constraints))}
    /*val cprdoDom3=  ProDom mapValues { p => (CStoPolyehdra(p.p1.dimension, p.p1.constraints) intersection(CStoPolyehdra(p.p2.dimension, p.p2.constraints)))}
    println(ProDom)
    println(cprdoDom1)
    println(cprdoDom2)
    println(cprdoDom3)*/
    //val cbox = Box mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
  //   val cpar = par mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
   totalTimeAltro=totalTimeAltro + tann2;
   totalTimeProd=totalTimeProd + tann5 ;
   // println(s"Times:  ${tann4} vs  ${tann6}")
   /* print("PPL Octagon_Shape_double: ")
    println(mkString(program, cann1))
    print("PPL C_Polyhedron: ")
    println(mkString(program, cann2)) */
   /* print("PTope: ")
    println(mkString(program, cann3))
    print("PTopeExt: ")
    println(mkString(program, cann4))*/
    /*print("Box: ")
    println(mkString(program, cbox))*/
    print("Altro: ")
    println(mkString(program, altro))
    print("ProdDomain: ")
    println(mkString(program, cprod))
   

    // SOSTITUIRE cann1 con cann3 se si vuole il confronto con i Parallelotopi.
    // val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann1(loc) intersection cann5(loc))) }

    //comparing sum with box
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(c ann1(loc))) }

    //comparing sum with parallelotope
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann3(loc))) }
    
    //comparing sum with parallelotope
  //  val comp = cprdoDom map { case (loc, v) => (loc ->{println(cprdoDom);println(cbox(loc) intersection(cpar(loc))); v.tryCompareTo(cbox(loc) intersection(cpar(loc)))}) }
    //val comp = cprdoDom  map { case (loc, v) => (loc ->v.tryCompareTo(cbox(loc) intersection(cpar(loc)))) }

    //val comp = cprdoDom  map { case (loc, v) => (loc ->v.tryCompareTo((cbox(loc)))) }
       /**
        * Confronto sui vincoli degli assi      
        */
   //  val comp = altro  map { case (loc, v) => (loc ->v.tryCompareTo((cann3(loc)))) }
    val comp = altro map { case (loc, v) => (loc ->v.tryCompareTo((cprod(loc)))) }
   
//  val comp = cprod map { case (loc, v) => (loc -> v.tryCompareTo(altro(loc) intersection cBox(loc))) }
    
    println("COUNT EQUALS: " + comp.count(_._2 == Some(0)))
    println("COUNT BETTER Altro: " + comp.count(_._2 == Some(-1)))
    println("COUNT BETTER ProdDom: " + comp.count(_._2 == Some(1)))
    println("COUNT UNCOMPARABLES: " + comp.count(_._2 == None))
//if(comp.count(_._2 == Some(1))>0) {Q += s"------>${model}"}
    totalEquals += comp.count(_._2 == Some(0))
    totalBestPPL += comp.count(_._2 == Some(-1))
    totalBestOther += comp.count(_._2 == Some(1))
    totalUncomparable += comp.count(_._2 == None)

    //println("DIFFERENT BEHAVIOURS: " + model)
    println(s"Times Altro Vs ProdDOm:  ${tann2} vs  ${tann5}")
    //println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
 
    println("EQUALS: " + program.locations.filter(comp(_) == Some(0)).map(_.name).mkString(", "))
    println("BETTER Altro: " + program.locations.filter(comp(_) == Some(-1)).map(_.name).mkString(", "))
    println("BETTER ProdDom: " + program.locations.filter(comp(_) == Some(1)).map(_.name).mkString(", "))
    println("UNCOMPARABLES: " + program.locations.filter(comp(_) == None).map(_.name).mkString(", "))
    println(s"Time Finish ${Calendar.getInstance().getTime()}")
  }  

val singolo=false
var badPrograms = Seq[File]()
if(singolo ==false){
  val resources = getClass.getResource("/fast/").toURI;
  for (model <- new File(resources).listFiles()) {
    try {
    //if(!model.getName.equals("berkeley.fst") && !model.getName.equals("halbwachs4.fst")) //Ptope(-1) delay n*3 // 2 to 
     // if(!model.getName.equals("multcounters3.fst") && !model.getName.equals("interleaving2.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("interleaving1.fst") && !model.getName.equals("gulwani2_alt.fst")) //C_Polyhedra  //
    //if(!model.getName.equals("multcounters3.fst")  && !model.getName.equals("interleaving2.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("interleaving1.fst") && !model.getName.equals("gulwani2_alt.fst") && !model.getName.equals("perfect.fst") && !model.getName.equals("gulwani2.fst") )
    
      fastModelAnalyze(model)
    } catch {
      case e: IllegalArgumentException =>
        badPrograms +:= model
    }
  }
}
else{//henry - speedometer // jeannet1
  //while2 --> delay =3
  //slam --> delay = 7
 val resources2 = getClass.getResource("/fast/loops.fst").toURI;
  val file= new File(resources2)  
   fastModelAnalyze(file) 
}
 
  
 //println("FINALE "+Q);

  

  // This analyzes all models (does not terminate for descending2 with

 

  /*
*/
  // This is if we want to analyze a specificic model
  // fastModelAnalyze(new File(resources.resolve("terminate.fst")))
//println("MappaStati "+mappaStati)
  
  println("\nFinal results:")
  println("---------------")
  println(s"Number of programs: ${totalPrograms}")
  println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
  println(s"Total equals: ${totalEquals}")
  println(s"Total best Altro: ${totalBestPPL}")
  println(s"Total best ProdDom: ${totalBestOther}")
  println(s"Total uncomparables: ${totalUncomparable}")
  print(s"Total time Altro: ${totalTimeAltro}  ")
  println(s"  vs    Total time ProdoRidotto: ${totalTimeProd}")
  println("Range Stati "+mappaStati.valuesIterator.min+ " -- " +mappaStati.valuesIterator.max)
   println("Range Variabili "+mappaVariabili.valuesIterator.min+ " -- " +mappaVariabili.valuesIterator.max)

}
* 
* 
*/
