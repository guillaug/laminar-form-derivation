package dev.cheleb.scalamigen.config

final case class PanelConfig(
    label: Option[String],
    asTable: Boolean,
    showField: Boolean = true,
    showBorder: Boolean = true,
    fieldCss: String = "srf-field",
    labelCss: String = "srf-label",
    panelCss: String = "srf-panel"
) {
  def withLabel(label: String): PanelConfig = copy(label = Some(label))
  def withAsTable(asTable: Boolean): PanelConfig = copy(asTable = asTable)
  def withFieldCss(fieldCss: String): PanelConfig = copy(fieldCss = fieldCss)
  def withLabelCss(labelCss: String): PanelConfig = copy(labelCss = labelCss)
  def withPanelCss(panelCss: String): PanelConfig = copy(panelCss = panelCss)
}
