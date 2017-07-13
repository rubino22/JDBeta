package it.unich.jandom

import java.io.File
import java.io.FileReader

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.parameters.IterationStrategy
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningSpecs._
import it.unich.jandom.domains.numerical.ppl.PPLDomainMacro
import parma_polyhedra_library.C_Polyhedron
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.BoxRationalDomain
//import it.unich.jandom.domains.numerical.ParallelotopeRationalDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.ppl.PPLDomain
import it.unich.jandom.domains.DimensionFiberedProperty
import java.util.Calendar
import parma_polyhedra_library.Double_Box
import it.unich.jandom.domains.numerical.ParallelotopeRationalDomainFast
import parma_polyhedra_library.Octagonal_Shape_double
import it.unich.jandom.targets.parameters.WideningNarrowingLocation
import parma_polyhedra_library.BD_Shape_double
import parma_polyhedra_library.Grid

object TestSCP17 extends App{

	def CStoPolyehdra(dimension: Int, c: Seq[LinearForm]) = {
		val d = PPLDomain[C_Polyhedron]
				c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm) => p.linearInequality(lf) }
	}

	def CStoBox(dimension: Int, c: Seq[LinearForm]) = {
		val d = PPLDomain[Double_Box]
				val poly = CStoPolyehdra(dimension, c)
				d(poly)
	}

	def mkString[U <: DimensionFiberedProperty[U]](program: LTS, m: scala.collection.Map[LTS#ProgramPoint, U]): String = {
		(for ((loc, prop) <- m) yield loc.name + " => " + prop.mkString(program.env.variables)).mkString(", ")
	}
	var totalEquals = 0
			var totalBestPPL = 0
			var totalBestOther = 0
			var totalUncomparable = 0
			var totalPrograms = 0
			var totalTimeAltro =0.0
			var totalTimeProd =0.0
			var mappaVariabili: scala.collection.immutable.Map[String,Int]=Map()
			var mappaStati: scala.collection.immutable.Map[String,Int]=Map()
			def fastModelAnalyze(model: File) = {
				println(s"------>${model}")


				val source = new FileReader(model)
				val parsed = FastParser().parse(source)
				source.close()
				val program = parsed.get

				println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
				var variabili=program.env.variables
				println("Variabili: " + program.env.variables.mkString(", "))
				//val delay=3* (program.env.variables.length)
				//  val tes= program.env.variables //3*variables
				// val tes = program.locations.filter(program.isJoinNode) //3+widening

				//  val delay= (tes.length *3)
				val delayWidening =0
				val delayNarrowing =0
				//println("lunghezza "+ program.locations.filter(program.isJoinNode).length)
				val nome= model.toString().split("/").map(_.trim).toList.last
				mappaVariabili+=(nome->program.env.variables.length)
				mappaStati +=(nome -> program.numlocs)

/* Domain poly*/
			val params = new targets.Parameters[LTS] { val domain = PPLDomainMacro[C_Polyhedron] }
   
				//val params = new targets.Parameters[LTS] { val domain = BoxRationalDomain() }
				//params.widening = DelayedWidening("H79", delay)
				//params.widening = DelayedWidening("BHRZ03", delay)
				params.widening = DelayedWidening(DefaultWidening, delayWidening)
				params.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)

						//params.iterationStrategy = IterationStrategy.Kleene
					//	params.debugWriter = new java.io.PrintWriter(System.out)
				     program.analyze(params) // warmup JVM
				//       params.debugWriter.flush()
				params.widening = DelayedWidening(DefaultWidening, delayWidening)
				//params.widening = DelayedWidening("BHRZ03", delay)
				params.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)
				//params.iterationStrategy = IterationStrategy.Kleene
				val t1 = System.currentTimeMillis
				//params.debugWriter = new java.io.PrintWriter(System.out)
				val ann1 = program.analyze(params)
				//params.debugWriter.flush()
				val tann1 = System.currentTimeMillis - t1
   

		/*
      //Test with other domain
     //   val params = new targets.Parameters[LTS] { val domain = ParallelotopeRationalDomainFast(-1) }
    //val params = new targets.Parameters[LTS] { val domain = new ProductDomain(ParallelotopeRationalDomainFast(1), ParallelotopeRationalDomainFast(-1)) }
    params.widening = DelayedWidening(DefaultWidening, delay)
    params.narrowing = DelayedNarrowing(DefaultNarrowing, delay)
   // params1.iterationStrategy = IterationStrategy.Kleene
   // params1.debugWriter = new java.io.PrintWriter(System.out)
     program.analyze(params) // warmup JVM
  // params1.debugWriter = new java.io.PrintWriter(System.out)
  // params1.debugWriter.flush()
   params.widening = DelayedWidening(DefaultWidening, delay)
    params.narrowing = DelayedNarrowing(DefaultNarrowing, delay)
    //params1.iterationStrategy = IterationStrategy.Kleene
    val t1 = System.currentTimeMillis
    val ann1 = program.analyze(params)
    val tann1 = System.currentTimeMillis - t1
				*/

    /*// BOX
      val params2 = new targets.Parameters[LTS] { val domain = BoxDoubleDomain() }
    //val params = new targets.Parameters[LTS] { val domain = new ProductDomain(ParallelotopeRationalDomainFast(1), ParallelotopeRationalDomainFast(-1)) }
    params2.widening = DelayedWidening(DefaultWidening, delay)
    params2.narrowing = DelayedNarrowing(DefaultNarrowing, delay)
   // params1.iterationStrategy = IterationStrategy.Kleene
   // params1.debugWriter = new java.io.PrintWriter(System.out)
     program.analyze(params2) // warmup JVM
  // params1.debugWriter = new java.io.PrintWriter(System.out)
  // params1.debugWriter.flush()
   params2.widening = DelayedWidening(DefaultWidening, delay)
    params2.narrowing = DelayedNarrowing(DefaultNarrowing, delay)
    //params1.iterationStrategy = IterationStrategy.Kleene
    val t12 = System.currentTimeMillis
    val ann12 = program.analyze(params2)
    val tann12 = System.currentTimeMillis - t12
    */

				/*
    params.iterationStrategy = IterationStrategy.Kleene
    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params)
    val tann2 = System.currentTimeMillis - t2
				 */
				// params.debugWriter.flush()

			//	val params1 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxRationalDomain(), ParallelotopeRationalDomainFast(-1)) }
    	  val params1 = new targets.Parameters[LTS] { val domain = ParallelotopeRationalDomainFast(1) }
			//val params1 = new targets.Parameters[LTS] { val domain = new ProductDomain(ParallelotopeRationalDomainFast(1), ParallelotopeRationalDomainFast(-1)) }
				params1.widening = DelayedWidening(DefaultWidening, delayWidening)
						params1.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)
						// params1.iterationStrategy = IterationStrategy.Kleene
					//	 params1.debugWriter = new java.io.PrintWriter(System.out)
						
						program.analyze(params1) // warmup JVM
					//	params1.debugWriter.flush()
				
						params1.widening = DelayedWidening(DefaultWidening, delayWidening)
						params1.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)
						//params1.iterationStrategy = IterationStrategy.Kleene
						val t3 = System.currentTimeMillis
						// params1.debugWriter = new java.io.PrintWriter(System.out)
						val productAnn = program.analyze(params1)
						// params1.debugWriter.flush()
						val tann3 = System.currentTimeMillis - t3


/**
 * 
 * su tutti i vincoli
 *  
 */
				   // val altro = ann1 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }  
					//val altrobox = ann12 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }   
			  //println("Pre trasformazione"+ productAnn)  
				//val cprod = productAnn mapValues { p =>{println("Dimensione="+p.dimension +"  Vincoli=" +p.constraints+" P="+p); println( CStoPolyehdra(p.dimension, p.constraints)); CStoPolyehdra(p.dimension, p.constraints)} }
           // val cprod = productAnn mapValues { p => CStoPolyehdra(p.dimension, p.constraints)}
		
			    	/*
			    	 * Proiettato sugli assi
			    	*/
			   	  val altro = ann1 mapValues { p => CStoBox(p.dimension, p.constraints) }  
					//val altrobox = ann12 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }   
			    	val cprod = productAnn mapValues { p => CStoBox(p.dimension, p.constraints) }
			    
									
			     

		/*
     val altro = ann1 mapValues { p => CStoBox(p.dimension, p.constraints) }    
     val cprod = productAnn mapValues { p => CStoBox(p.dimension, p.constraints) }
   */
   
				totalTimeAltro=totalTimeAltro + tann1;
				totalTimeProd=totalTimeProd + tann3 ;

				print("Altro: ")
				println(mkString(program, altro))
				print("ProdDomain: ")
				println(mkString(program, cprod))


				val comp = altro map { case (loc, v) => (loc ->v.tryCompareTo((cprod(loc)))) }
			 //	val comp = altro map { case (loc, v) => (loc -> v.tryCompareTo(cprod(loc) intersection altrobox(loc))) }

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
				println(s"Times Altro Vs ProdDOm:  ${tann1} vs  ${tann3}")
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
								// if(!model.getName.equals("multcounters3.fst")  && !model.getName.equals("interleaving2.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("interleaving1.fst") && !model.getName.equals("gulwani2_alt.fst") && !model.getName.equals("perfect.fst") && !model.getName.equals("gulwani2.fst") )

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
						val resources2 = getClass.getResource("/fast/metro.fst").toURI;
						val file= new File(resources2)  
						fastModelAnalyze(file) 
					}


			println("\nFinal results:")
			println("---------------")
			println(s"Number of programs: ${totalPrograms}")
			println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
			println(s"Total equals: ${totalEquals}")
			println(s"Total best Altro: ${totalBestPPL+totalUncomparable}")
			println(s"Total best ProdDom: ${totalBestOther+totalUncomparable}")
			println(s"Total uncomparables: ${totalUncomparable}")
			print(s"Total time Altro: ${totalTimeAltro}  ")
			println(s"  vs    Total time ProdoRidotto: ${totalTimeProd}")
			println("Range Stati "+mappaStati.valuesIterator.min+ " -- " +mappaStati.valuesIterator.max)
			println("Range Variabili "+mappaVariabili.valuesIterator.min+ " -- " +mappaVariabili.valuesIterator.max)
}