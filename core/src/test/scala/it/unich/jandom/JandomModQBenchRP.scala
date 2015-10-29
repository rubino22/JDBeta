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
import it.unich.jandom.widenings.DefaultWidening
import parma_polyhedra_library.C_Polyhedron
import it.unich.jandom.domains.numerical.ParallelotopeDomainModRationalGmpExt
import it.unich.jandom.utils.numberext.ModRationalGmpExt
import parma_polyhedra_library.PPL_Object
import it.unich.jandom.domains.numerical.ppl.PPLDomainSuiteOctagon
import parma_polyhedra_library.Octagonal_Shape_double
import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.domains.numerical.ParallelotopeDomainModQSpire
import it.unich.jandom.targets.slil.SLILTarget
import it.unich.jandom.parsers.RandomParser
import it.unich.jandom.targets.slil.SLILProgram
import it.unich.jandom.targets.slil.SLILPrinterSpecOffline
import it.unich.jandom.targets.NarrowingStrategy
import it.unich.jandom.targets.WideningScope
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.ppl.PPLBoxDoubleDomain
import parma_polyhedra_library.Double_Box
import it.unich.jandom.domains.numerical.BoxSpireDomain


/**
 * Example program using ''Jandom'' to analyze the Alice benchmarks and
 * compare the resuSLILTarget with different parameters. In this moment, it compares
 * the result of the analyisis with standard Kleene iteration and worklist
 * based ones.
 */
object JandomModQBenchRP extends App {

	var totalEquals = 0
			var totalBestPPL = 0
			var totalBestOther = 0
			var totalUncomparable = 0
			var totalPrograms = 0
			var totalTimeGMP =0.0
			var totalTimeSpire =0.0

			def CStoPolyehdra(dimension: Int, c: Seq[LinearForm[Double]]) = {
		val d = PPLDomain[C_Polyhedron]    
				c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm[Double]) => p.linearInequality(lf) }
	}

	def mkString[U <: DimensionFiberedProperty[U]](program: SLILProgram, m: scala.collection.Map[SLILTarget#ProgramPoint, U]): String = {
		(for ((loc, prop) <- m) yield loc._2 + " => " + prop.mkString(program.env.variables)).mkString(", ")
	}

	def fastModelAnalyze(model: File) = {
		totalPrograms += 1

				println(s"------>${model}")

				val fr = new FileReader(model)   
		val source = scala.io.Source.fromFile(model.getAbsolutePath).getLines.mkString("\n")
		val parsed = parsers.RandomParser().parseProgram(source)


		fr.close()


	
		val program= parsed.get
		val params = new targets.Parameters[SLILTarget] { val domain = ParallelotopeDomainModQSpire(1,overRound=false)}    
		    params.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) 
				params.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
       
				program.analyze(params) // warmup JVM


				val t = System.currentTimeMillis
				val p1 = program.analyze(params)
				val tann = System.currentTimeMillis - t

	/*	val params1 = new targets.Parameters[SLILTarget] { val domain = PPLDomain[Octagonal_Shape_double]()}
		    params1.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopes
				params1.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3) // needed for parallelotopes

				val t1 = System.currentTimeMillis
				val octagon = program.analyze(params1)
				val tann1 = System.currentTimeMillis - t1

*/
		val params6 = new targets.Parameters[SLILTarget] { val domain = new ProductDomain(BoxSpireDomain(),ParallelotopeDomainModQSpire(-1,overRound=false)) }
		    params6.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
				params6.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
       //  params6.narrowingStrategy = NarrowingStrategy.Restart
      // params6.wideningScope = WideningScope.BackEdges
				//params4.debugWriter = new java.io.PrintWriter(System.out)

				program.analyze(params6)// warmup JVM
				//params4.debugWriter.flush()

				val t6 = System.currentTimeMillis
				val p2 = program.analyze(params6)
				val tann6 = System.currentTimeMillis - t6

   /* val params7 = new targets.Parameters[SLILTarget] { val domain = new ProductDomain(BoxDoubleDomain(),ParallelotopeDomainModQSpire(favorAxes=false,overRound=false)) }
        params7.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
        params7.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    
        program.stmt.analyze(params7) // warmup JVM
    

        val t7 = System.currentTimeMillis
        val prodDom= program.stmt.analyze(params7)
        val tann7 = System.currentTimeMillis - t7    

      
    val params8 = new targets.Parameters[SLILTarget] { val domain = new ProductDomain(PPLDomain[C_Polyhedron](),PPLDomain[Octagonal_Shape_double]()) }
        params8.wideningFactory = DelayedWideningFactory(DefaultWidening, 3) // needed for parallelotopesModQExt
        params8.narrowingFactory = DelayedNarrowingFactory(DefaultNarrowing, 3)
    
        program.stmt.analyze(params8) // warmup JVM
    

        val t8 = System.currentTimeMillis
        val prodDom2= program.stmt.analyze(params8)
        val tann8 = System.currentTimeMillis - t8        
        */
	//	val  coctagon= octagon mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
	//	val  cpoly= poly mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
	//	val  cspire= spire mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
 //   val  test= prodDom2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
  //  val  cprodDom = prodDom mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
        
        val  cp1 = p1 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
        val  cp2 = p2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }

	/*	print("PPL Octagon_Shape_double: ")
		println(mkString(program, coctagon))
		print("PPL C_Polyhedron: ")
		println(mkString(program, cpoly))
		print("PTope: ")
		println(mkString(program, cspire))*/
   /* print("TEST: ")
    println(mkString(program, test))*/ 
   /* print("ProdDom: ")
    println(mkString(program, cprodDom))*/
        
    print("Ptope: ")
    println(mkString(program, cp1)) 
     print("ProdDom: ")
    println(mkString(program, cp2))


		//comparing sum with parallelotope
		val comp = cp1 map { case (loc, v) => (loc -> v.tryCompareTo(cp2(loc))) }

		println("COUNT EQUALS: " + comp.count(_._2 == Some(0)))
		println("COUNT BETTER OCT: " + comp.count(_._2 == Some(-1)))
		println("COUNT BETTER OTHER: " + comp.count(_._2 == Some(1)))
		println("COUNT UNCOMPARABLES: " + comp.count(_._2 == None))

		totalEquals += comp.count(_._2 == Some(0))
		totalBestPPL += comp.count(_._2 == Some(-1))
		totalBestOther += comp.count(_._2 == Some(1))
		totalUncomparable += comp.count(_._2 == None)

		//println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))

/*		println("EQUALS: " + program.lastPP.filter(comp(_) == Some(0)).mkString(", "))
		println("BETTER PPL: " + program.lastPP.filter(comp(_) == Some(-1)).mkString(", "))
		println("BETTER OTHER: " + program.lastPP.filter(comp(_) == Some(1)).mkString(", "))
		println("UNCOMPARABLES: " + program.lastPP.filter(comp(_) == None).mkString(", "))
*/
	}

// run single program
/*	val resources2 = getClass.getResource("/random/marco1.R").toURI;
  val file= new File(resources2)
   fastModelAnalyze(file)  
*/
	var badPrograms = Seq[File]()

			// This analyzes all models (does not terminate for descending2 with
		
			val resources = getClass.getResource("/random").toURI()
			

		for (model <- new File(resources).listFiles()) {
				try {
					fastModelAnalyze(model)
				} catch {
				case e: IllegalArgumentException =>
				badPrograms +:= model
				}
			}

/* for (model <- new File(resources).listFiles()) {
       val ptope=fastModelAnalyze(model)
    }*/

	// This is if we want to analyze a specificic model
	// fastModelAnalyze(new File(resources.resolve("terminate.fst")))

	println("\nFinal results:")
	println("---------------")
	println(s"Number of programs: ${totalPrograms}")
	println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
	println(s"Total equals: ${totalEquals}")
	println(s"Total best Ptope: ${totalBestPPL}")
	println(s"Total best ProDom: ${totalBestOther}")
	println(s"Total uncomparables: ${totalUncomparable}")


}
