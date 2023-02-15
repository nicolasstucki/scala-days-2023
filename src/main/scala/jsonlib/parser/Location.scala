package jsonlib
package parser

/** Location in a string interpolation.
 *
 *  @param partIndex index of the string in `StringContext.parts`
 *  @param offset character index in the `StringContext` part
 */
private[jsonlib] class Location(val partIndex: Int, val offset: Int)
