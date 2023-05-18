package samples

import dev.cheleb.scalamigen.*
import dev.cheleb.scalamigen.forms.given
import dev.cheleb.scalamigen.forms.*

import com.raquo.laminar.api.L.*

import com.raquo.airstream.state.Var

val either = {

  case class EitherSample(
      either: Either[Cat, Dog],
      optionalInt: Option[Int]
  )

  val eitherVar = Var(EitherSample(Left(Cat("Scala le chat", 6)), Some(1)))

  div(
    child <-- eitherVar.signal.map { item =>
      div(
        s"$item"
      )
    },
    Form.renderVar(eitherVar)
  )
}
