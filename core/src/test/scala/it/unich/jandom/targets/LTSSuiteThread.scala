/**
 * Copyright 2013, 2015 Gianluca Amato
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.targets

import java.io.File
import java.io.FileReader
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.lts._
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.FixpointSolver._


class LTSSuiteThread extends FunSuite {
 val dom = BoxDoubleDomain()

  implicit val scalafixDomain = dom.ScalaFixDomain
  val wideningBox = { (x: dom.Property, y: dom.Property) => x widening y }
  val narrowingBox = { (x: dom.Property, y: dom.Property) => x narrowing y }
  val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.RoundRobinSolver, wideningBox, narrowingBox)

 
  val dir = new File(getClass.getResource("/fast/").toURI);
  for (model <- dir.listFiles()) {

    val fr = new FileReader(model)
    val source = new PagedSeqReader(PagedSeq.fromReader(fr))
    val result = FastParser().parse(source)
    fr.close()
   //lts.locations.zipWithIndex.filter{ _._2 >= lts.locations.size/2}.map { _._1}
    val lts = result.getOrElse(fail(result.toString))
    //var lt1= lts.toEQS(dom)
    test(s"compare LTS analsysis for ${lts.name} in file ${model}") {
      val params = new Parameters[LTS] { val domain = dom }
      
      
      val ann1 = lts.analyze(params)
      val ltssplit= lts.locations.splitAt(lts.locations.size/2)
      val ann2 = FiniteFixpointSolver(lts.toEQS(dom), CC77)
     

    
        //println("Lista "+lts.locations+ " split-> "+ltssplit._1+ " ->"+ltssplit._2);
      //val ann3 = FiniteFixpointSolver(lts.toEQS(dom),CC77)
      println("location "+lts.toEQS(dom).unknowns+" number: "+ lts.toEQS(dom).unknowns.size);
      for (l <- lts.locations) assert(ann1(l) === ann2(l))
    }
  }
}
