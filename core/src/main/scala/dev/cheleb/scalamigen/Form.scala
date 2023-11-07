package dev.cheleb.scalamigen

import com.raquo.laminar.api.L.*
import magnolia1.*
import scala.CanEqual.derived
import java.util.UUID
import scala.util.Random

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import scala.util.Try
import com.raquo.airstream.state.Var
import com.raquo.airstream.core.Source
import com.raquo.laminar.nodes.ReactiveElement
import org.scalajs.dom.HTMLDivElement
import magnolia1.SealedTrait.SubtypeValue

trait IronTypeValidator[T, C] {
  def validate(a: String): Either[String, IronType[T, C]]
}

trait Defaultable[A] {
  def default: A
  def label: String = default.getClass.getSimpleName()
}

trait Form[A] { self =>

  def isAnyRef = false

//  def default: A

  def fromString(s: String): Option[A] = None
  def fromString(s: String, variable: Var[A], errorVar: Var[String]): Unit = ()

  def toString(a: A) = a.toString

  def render(
      variable: Var[A],
      syncParent: () => Unit,
      values: List[A] = List.empty
  ): HtmlElement =
    val errorVar = Var("")
    div(
      div(child <-- errorVar.signal.map { item =>
        div(
          s"$item"
        )
      }),
      input(
        // _.showClearIcon := true,
        backgroundColor <-- errorVar.signal.map {
          case "" => "white"
          case _  => "red"
        },
        value <-- variable.signal.map(toString(_)),
        onInput.mapToValue --> { str =>
          fromString(str, variable, errorVar)

        }
      )
    )

  extension (a: A)
    def render: HtmlElement =
      self.render(Var(a), () => ())

  given Owner = unsafeWindowOwner

  def labelled(name: String, required: Boolean): Form[A] = new Form[A] {
    override def render(
        variable: Var[A],
        syncParent: () => Unit,
        values: List[A] = List.empty
    ): HtmlElement =
      div(
        div(
          label(
            cls := "block text-sm font-medium leading-6 text-gray-900",
            name
          )
        ),
        div(
          self.render(variable, syncParent, values)
        )
      )

  }
  def xmap[B](to: (B, A) => B)(from: B => A): Form[B] = new Form[B] {
    override def render(
        variable: Var[B],
        syncParent: () => Unit,
        values: List[B] = List.empty
    ): HtmlElement =
      self.render(variable.zoom(from)(to), syncParent, values.map(from))
  }

}

object Form extends AutoDerivation[Form] {

  def renderVar[A](v: Var[A], syncParent: () => Unit = () => ())(using
      fa: Form[A]
  ) =
    fa.render(v, syncParent)

  def join[A](caseClass: CaseClass[Typeclass, A]): Form[A] = new Form[A] {

    override def isAnyRef: Boolean = true
    override def render(
        variable: Var[A],
        syncParent: () => Unit = () => (),
        values: List[A] = List.empty
    ): HtmlElement =
      div(
        idAttr := caseClass.typeInfo.full,
        cls := "divide-y divide-gray-200 overflow-hidden rounded-lg bg-white shadow",
        div(
          cls := "px-4 py-5 sm:px-6",
          caseClass.typeInfo.full
        ),
        div(
          cls := "px-4 py-5 sm:p-6",
          caseClass.params.map { param =>
            val isOption = param.deref(variable.now()).isInstanceOf[Option[_]]

            val enumValues =
              if param.annotations.isEmpty then List.empty[A]
              else if param.annotations(0).isInstanceOf[EnumValues[_]] then
                param.annotations(0).asInstanceOf[EnumValues[A]].values.toList
              else List.empty[A]

            param.typeclass
              .labelled(param.label, !isOption)
              .render(
                variable.zoom(a => param.deref(a))((_, value) =>
                  caseClass.construct { p =>
                    if (p.label == param.label) value
                    else p.deref(variable.now())
                  }
                )(unsafeWindowOwner),
                syncParent,
                enumValues.map(_.asInstanceOf[param.PType])
              )
              .amend(
                idAttr := param.label
              )
          }.toSeq
        )
      )
  }

  def split[A](sealedTrait: SealedTrait[Form, A]): Form[A] = new Form[A] {

    override def isAnyRef: Boolean = true
    override def render(
        variable: Var[A],
        syncParent: () => Unit,
        values: List[A] = List.empty
    ): HtmlElement =
      if sealedTrait.isEnum then
        val valuesLabels = values.map(_.toString)
        div(
          select(
            // idAttr := "location",
            // name := "location",
            cls := "mt-2 block w-full rounded-md border-0 py-1.5 pl-3 pr-10 text-gray-900 ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-indigo-600 sm:text-sm sm:leading-6",
            sealedTrait.subtypes.toSeq
              .map(_.typeInfo.short)
              .filter(valuesLabels.contains(_))
              .map { label =>
                option(
                  label,
                  dataAttr("idx") := values
                    .map(_.toString)
                    .indexOf(label)
                    .toString(),
                  selected <-- variable.signal.map(
                    _.toString() == label
                  )
                )
              }
          )
          // Select(
          //   _.events.onChange
          //     .map(_.detail.selectedOption.dataset) --> { ds =>
          //     ds.get("idx").foreach(idx => variable.set(values(idx.toInt)))
          //     syncParent()
          //   },
          //   sealedTrait.subtypes
          //     .map(_.typeInfo.short)
          //     .filter(valuesLabels.contains(_))
          //     .map { label =>
          //       Select.option(
          //         label,
          //         dataAttr("idx") := values
          //           .map(_.toString)
          //           .indexOf(label)
          //           .toString(),
          //         _.selected <-- variable.signal.map(
          //           _.toString() == label
          //         )
          //       )
          //     }
          //     .toSeq
          // )
        )
      else div("Not an enum")

  }

}
