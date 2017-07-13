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
import it.unich.jandom.domains.numerical.ppl.PPLProperty
import it.unich.jandom.domains.numerical.ppl.PPLPropertyMacro
import scala.collection.mutable.ArrayBuffer

import scala.collection.mutable.Map
import it.unich.jandom.utils.numberext.RationalExt
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.immutable.ListMap
import java.util.Formatter.DateTime

import scala.concurrent._
import java.util.concurrent.FutureTask
import java.util.concurrent.Executor
import java.util.concurrent.Callable



object PaperJuly17PolyVsProd extends App {

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
			var totalTimePoly79 =0.0
			var totalTimePoly03 =0.0
			var totalTimeBox =0.0
			var totalTimeGrid =0.0
			var totalTimePar =0.0
			var totalTimeOct =0.0
			var totalTimeProdDom =0.0
			//var mappaVariabili: scala.collection.immutable.Map[String,Int]=Map()
			//var mappaStati: scala.collection.immutable.Map[String,Int]=Map()
			val listaNomi : Array[String] = Array("MapPoly79", "MapPoly03","MapProDom","MapBox","MapPar")
				
				var finaleCompare: scala.collection.mutable.Map[String,Int]= Map(("MapPoly79",0), ("MapPoly03",0),("MapProDom",0),("MapBox",0),("MapPar",0))

			
			def fastModelAnalyze(model: File) = {
				println(s"------>${model}")
     var parzialeCompare: scala.collection.mutable.Map[String,Int]= Map(("MapPoly79",0), ("MapPoly03",0),("MapProDom",0),("MapBox",0),("MapPar",0))

				val source = new FileReader(model)
				val parsed = FastParser().parse(source)
				source.close()
				val program = parsed.get

				println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
				var variabili=program.env.variables
				println("Variabili: " + program.env.variables.mkString(", "))
			
				val delayWidening =2
				
				val delayNarrowing =3
				//println("lunghezza "+ program.locations.filter(program.isJoinNode).length)
				val nome= model.toString().split("/").map(_.trim).toList.last
			//	mappaVariabili+=(nome->program.env.variables.length)
				//mappaStati +=(nome -> program.numlocs)
      
        val modello= model.toString().split("/")(10);
				/* Domain poly*/
				println("TIME Begine POLY79: "+Calendar.getInstance.getTime+" Model: "+modello)

				//var MapPoly79: scala.collection.Map[program.ProgramPoint,it.unich.jandom.domains.numerical.ppl.PPLProperty[parma_polyhedra_library.Double_Box]]  = Map.empty
			//	var resultP79 = 0.0;
				//if(jump){
				
				
				val params = new targets.Parameters[LTS] { val domain = PPLDomainMacro[C_Polyhedron] }

				    params.widening = DelayedWidening("H79", delayWidening)
						params.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)

             program.analyze(params)// warmup JVM


						params.widening = DelayedWidening("H79", delayWidening)
						params.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)	
						val t0P79 = System.currentTimeMillis
						println("TIME Begine Post POLY79: "+Calendar.getInstance.getTime+" Model: "+modello)
				   //params.debugWriter = new java.io.PrintWriter(System.out)
						val polyH79 = program.analyze(params)       
						//   	params.debugWriter.flush()
				    val resultP79 = System.currentTimeMillis() - t0P79
            println("Elapsed time Poly79: " + (resultP79) + "ns")
				    val MapPoly79  = polyH79 mapValues { p => CStoBox(p.dimension, p.constraints) }
			//	}
				    
				    
				    
						val params2 = new targets.Parameters[LTS] { val domain = PPLDomainMacro[C_Polyhedron] }

	println("TIME Begine PRE POLY03: "+Calendar.getInstance.getTime+" Model: "+modello)
				    params2.widening = DelayedWidening("BHRZ03", delayWidening)				
						params2.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)				
						program.analyze(params2) // warmup JVM

						params2.widening = DelayedWidening("BHRZ03", delayWidening)
						params2.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)				
						val t0P03 = System.currentTimeMillis()
						println("TIME Begine Post POLY03: "+Calendar.getInstance.getTime+" Model: "+modello)
						 params2.debugWriter = new java.io.PrintWriter(System.out)
						val polyBhrz03 = program.analyze(params2)
					params2.debugWriter.flush()
            //val t1P03 = System.currentTimeMillis()
				    val resultP03 = System.currentTimeMillis() - t0P03
            println("Elapsed time Poly03: " + (resultP03) + "ns")

						

						val params4 = new targets.Parameters[LTS] { val domain = new ProductDomain(BoxRationalDomain(), ParallelotopeRationalDomainFast(-1)) }

				    params4.widening = DelayedWidening(DefaultWidening, delayWidening)
						params4.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)	
						program.analyze(params4) // warmup JVM

						params4.widening = DelayedWidening(DefaultWidening, delayWidening)
						params4.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)
						val t0ProdDom = System.currentTimeMillis()
						val proDom = program.analyze(params4)
						//val t1ProdDom = System.currentTimeMillis()
				    val resultProdDom = System.currentTimeMillis() - t0ProdDom
            println("Elapsed time ProDOm: " + (resultProdDom) + "ns")

		      
						val params7 = new targets.Parameters[LTS] { val domain = ParallelotopeRationalDomainFast(1) }

				    params7.widening = DelayedWidening(DefaultWidening, delayWidening)
						params7.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)	
						program.analyze(params7) // warmup JVM

						params7.widening = DelayedWidening(DefaultWidening, delayWidening)
						params7.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)
							val t0Par = System.currentTimeMillis()
						val par = program.analyze(params7)
						//val t1Par = System.currentTimeMillis()
				    val resultPar = System.currentTimeMillis() - t0Par
            println("Elapsed time Par: " + (resultPar) + "ns")
						
				    
				    
				   		// BOX
						val params3 = new targets.Parameters[LTS] { val domain = BoxRationalDomain() }
				//val params = new targets.Parameters[LTS] { val domain = new ProductDomain(ParallelotopeRationalDomainFast(1), ParallelotopeRationalDomainFast(-1)) }
			     	params3.widening = DelayedWidening(DefaultWidening, delayWidening)
						params3.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)  
						program.analyze(params3) // warmup JVM

						params3.widening = DelayedWidening(DefaultWidening, delayWidening)
						params3.narrowing = DelayedNarrowing(DefaultNarrowing, delayNarrowing)  
						val t0Box = System.currentTimeMillis()
						val box = program.analyze(params3)
           // val t1Box = System.currentTimeMillis()
				    val resultBox = System.currentTimeMillis() - t0Box
            println("Elapsed time Box: " + (resultBox) + "ns")

				    
				    
				    
					 val MapPoly03 = polyBhrz03 mapValues { p => CStoBox(p.dimension, p.constraints) }			  
				   val MapProDom = proDom mapValues { p => CStoBox(p.dimension, p.constraints) }				   
				   val MapPar = par mapValues { p => CStoBox(p.dimension, p.constraints) }
				    val MapBox = box mapValues { p => CStoBox(p.dimension, p.constraints) }



				   
				   


				print("PolyH79: ")
				println(mkString(program, MapPoly79))
				print("Poly03: ")
				println(mkString(program, MapPoly03))
        print("ProDom: ")
				println(mkString(program, MapProDom))				
				print("Paralletope: ")
				println(mkString(program, MapPar))
				print("Box: ")
				println(mkString(program, MapBox))

				
				
				val myArray2 : Array[scala.collection.Map[program.ProgramPoint,it.unich.jandom.domains.numerical.ppl.PPLProperty[parma_polyhedra_library.Double_Box]]]   = Array(MapPoly79, MapPoly03,MapProDom,MapBox,MapPar)			
		    var mapCompo = scala.collection.mutable.Map[String, scala.collection.Map[program.ProgramPoint,Option[Int]]]()
		    var test = scala.collection.mutable.Map[String,scala.collection.Map[LinearForm, scala.collection.Map[RationalExt,RationalExt]]]()
			
		     val modelliMax = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, HashMap[String, RationalExt]]]()
		     val modelliMin = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, HashMap[String, RationalExt]]]()
				for(j <- 0 until myArray2.length ){
				    var variabile= listaNomi(j)
				  //println("---->"+variabile)
				   val pmax = scala.collection.mutable.Map[String, HashMap[String, RationalExt]]()
				   val pmin = scala.collection.mutable.Map[String, HashMap[String, RationalExt]]()
				    for ((k,v) <- myArray2(j)){
                 //printf("key: %s, value: %s\n", k, v)
                
               //  val finaleTemp = scala.collection.mutable.Map[LinearForm, Map[String,RationalExt]]()
                  val minimo = new HashMap[String, RationalExt]
                 val massimo = new HashMap[String,RationalExt]
                 //println("DIMENSIONE "+v.dimension)
             for(i <- 0 until v.dimension){
                val tempRatinMin  = scala.collection.mutable.Map[String,RationalExt]()
                 val tempRatinMax  = scala.collection.mutable.Map[String,RationalExt]()
               //tempRatinMin += ("Min" -> v.minimize(LinearForm.v(i)))
               //tempRatinMax += ("Max" -> v.maximize(LinearForm.v(i)))         
               minimo +=(LinearForm.v(i).toString() -> v.minimize(LinearForm.v(i)))
               massimo +=(LinearForm.v(i).toString() -> v.maximize(LinearForm.v(i)))
               
              
               pmax +=(k.toString() -> massimo)
                pmin +=(k.toString() -> minimo)
             }
                 
             //println(pp)
}
				
				modelliMax += (variabile ->  pmax)
				modelliMin += (variabile ->  pmin)
				}
				/*
				       for ((k,v) <- myArray2(0)){ // per ogni locazione del dominio
				for (l<- 0 until modelli.get(listaNomi(z)).get(k).size){
				  println(listaNomi(l))
				   println(modelli.get(listaNomi(l)).toString())
				   println(modelli.get(listaNomi(l)).get(k).toString())
				   
				  println("min ==> "+ modelli.get(listaNomi(l)).get(k).get(LinearForm.v(l)).getOrElse("Min"))
				  
				}
				
				  }*/
			
				    //   println(program.locations)
				         val Ar=program.locations.toArray
				   // println("ARRRAY"+ program.env.variables.size)
				         // val min = scala.collection.mutable.Map[String, RationalExt]()
				         // val max = scala.collection.mutable.Map[String, RationalExt]()
				         for(y <- 0 until program.env.variables.size){
				              for(x <- 0 until program.locations.size ){ // per ogni program point
				           var CompMax = scala.collection.mutable.Map[String, RationalExt]()
				           var CompMin = scala.collection.mutable.Map[String, RationalExt]()
				          for(z <- 0 until listaNomi.size){
				          
				           //modelli.get(listaNomi(z)).get(y.toString()).get(LinearForm.v(x)).ge
				           //for ((k,v) <- modelliMin) printf("MIN KEY, value: %s\n", k, v)
				           //  for ((k,v) <- modelliMax) printf("MAX key: %s, value: %s\n", k, v)
				//            println("xxxxx====>>>"+modelliMin.get(listaNomi(z)).get(Ar(x).toString())+ "  xxxx ="+LinearForm.v(y) )
				           // if(!(modelliMin.get(listaNomi(z)).get(Ar(y).toString()).get(LinearForm.v(x).toString()).eq("None"))){
				           //   println("xxxrrrxx====>>>"+modelliMin.get(listaNomi(z)).get(Ar(y).toString()).get(LinearForm.v(x).toString())+ "  xxxx ="+LinearForm.v(x) )
				            
				            if(!modelliMin.get(listaNomi(z)).get(Ar(x).toString()).get(LinearForm.v(y).toString()).get.isInfinity){
				        CompMin += (listaNomi(z)  -> modelliMin.get(listaNomi(z)).get(Ar(x).toString()).get(LinearForm.v(y).toString()).get)
				          }
				            if(!modelliMax.get(listaNomi(z)).get(Ar(x).toString()).get(LinearForm.v(y).toString()).get.isInfinity){
				        CompMax += (listaNomi(z)  -> modelliMax.get(listaNomi(z)).get(Ar(x).toString()).get(LinearForm.v(y).toString()).get)
				            }
				           // }
				           }
				           var lMax = ListMap(CompMax.toSeq.sortWith(_._2 < _._2):_*)
				           var lMin = ListMap(CompMin.toSeq.sortWith(_._2 > _._2):_*)
				           for(t <- 0 until  lMax.size ){
				             
				             
				           }
				           var flag=true;
				           var primo =RationalExt.zero;
				           for ((k,v) <- lMax){ 
				            // printf("key: %s, value: %s\n", k, v)
				             
				             if(flag){
				               primo=v;
				               flag=false
				             }
				             if(v ==  primo){
				               finaleCompare(k) +=1
				               parzialeCompare(k) +=1
				             }
				           }
				             flag=true;
				           primo =RationalExt.zero;
				           for ((k,v) <- lMin){ 
				             //printf("key: %s, value: %s\n", k, v)
				             
				             if(flag){
				               primo=v;
				               flag=false
				             }
				             if(v ==  primo){
				               finaleCompare(k) +=1
				               parzialeCompare(k) +=1
				             }
				           }
				         //  println(CompMax +" Massimo="+lMax) //aprire qui
				         //  println(CompMin + " MInimo="+lMin) //aprire qui
				         }			     //CICLO SU TUTTI I MODELLI 
				       }	// CICLO PP
				         
			 totalTimePoly79 =totalTimePoly79 + resultP79;
			 totalTimePoly03 =totalTimePoly03 + resultP03	
			 totalTimePar  = totalTimePar + resultPar			
			 totalTimeProdDom = totalTimeProdDom + resultProdDom
				         
				       			         for ((k,v) <- parzialeCompare) printf("Parziale Key: %s, value: %s\n", k, v)
			}


			val singolo=true
					var badPrograms = Seq[File]()
					if(singolo ==false){
						val resources = getClass.getResource("/fast/").toURI;
						for (model <- new File(resources).listFiles()) {
							try {
								//if(!model.getName.equals("berkeley.fst") && !model.getName.equals("halbwachs4.fst")) //Ptope(-1) delay n*3 // 2 to 
								// if(!model.getName.equals("interleaving1.fst") && !model.getName.equals("multcounters3.fst") && !model.getName.equals("multcounters4.fst") && !model.getName.equals("gulwani2_alt.fst") && !model.getName.equals("interleaving2.fst") && !model.getName.equals("perfect.fst")  && !model.getName.equals("gulwani2.fst") ) //C_Polyhedra  //
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
						val resources2 = getClass.getResource("/fast/berkeleyModify.fst").toURI;
					  //val resources2 = getClass.getResource("/fast/slam.fst").toURI;
						val file= new File(resources2)  
						fastModelAnalyze(file) 
					}


			println("\nFinal results:")
			println("---------------")
			for ((k,v) <- finaleCompare) printf("%s= %s\n", k, v)
			/*println("TOTAL TIME" )
			println(s"Poly79: ${totalTimePoly79}  ")
			println(s"Poly03: ${totalTimePoly03}  ")
			println(s"Par: ${totalTimePar}  ")
			println(s"ProdDom: ${totalTimeProdDom}  ")
			println(s"Box: ${totalTimeBox}  ")
			println(s"Grid: ${totalTimeGrid}  ")
			println(s"Oct: ${totalTimeOct}  ")*/
		/*	println(s"Number of programs: ${totalPrograms}")
			println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
			println(s"Total equals: ${totalEquals}")
			println(s"Total best Altro: ${totalBestPPL}")
			println(s"Total best ProdDom: ${totalBestOther}")
			println(s"Total uncomparables: ${totalUncomparable}")
			print(s"Total time Altro: ${totalTimeAltro}  ")
			println(s"  vs    Total time ProdoRidotto: ${totalTimeProd}")*/
		//	println("Range Stati "+mappaStati.valuesIterator.min+ " -- " +mappaStati.valuesIterator.max)
		//	println("Range Variabili "+mappaVariabili.valuesIterator.min+ " -- " +mappaVariabili.valuesIterator.max)
}