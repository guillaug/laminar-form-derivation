package demo

import org.scalajs.dom
import com.raquo.laminar.api.L.*

object App extends App {

  val sample = Var(samples.person)

  val myApp =
    div(
      display := "flex",
      div(
        paddingRight("2rem"),
        div(
          "Demos",
          padding("0.5rem"),
          cursor := "pointer"
        ),
        div(
            div(
              "Person",
              dataAttr("component-name") := "Person"
            )
          )
      ),
      div(
        height := "100vh",
        overflowY := "auto",
        display := "flex",
        flexGrow := 1,
        div(
          padding := "10px",
          minWidth := "40%",
          maxWidth := "calc(100% - 320px)",
          child <-- sample.signal
        )
      )
    )

  val containerNode = dom.document.getElementById("root")
  render(containerNode, myApp)
}
