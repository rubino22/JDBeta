/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom
package targets

/**
 * The class representing a variable. At the moment, this is dead code.
 * @param name the name of the variable
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class Variable (val name: String) { }

/**
 * Factory object for the Variable class.
 * 
 * This object provides a set of operations to create Variable values. 
 */
object Variable {
  def apply(name: String) = new Variable(name)
} 