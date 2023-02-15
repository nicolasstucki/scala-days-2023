package jsonlib.util

import scala.util.boundary

sealed trait Result[+T, +E]
final case class Success[+T](value: T) extends Result[T, Nothing]
final case class Error[+E](reason: E) extends Result[Nothing, E]

object Result:

  def withContinuation[T, E](x: Continuation[T, E] ?=> T): Result[T, E] =
    boundary { label ?=>
      Success(x(using Continuation(label)))
    }

  final class Continuation[T, E] private[Result](private[Result] val label: boundary.Label[Result[T, E]])

  object continuation:
    def error[T, E](error: E)(using continuation: Continuation[T, E]): Nothing =
      boundary.break(Error(error))(using continuation.label)

    def success[T, E](value: T)(using continuation: Continuation[T, E]): Nothing =
      boundary.break(Success(value))(using continuation.label)
