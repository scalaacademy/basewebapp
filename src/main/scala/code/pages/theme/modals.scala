package code.pages.theme

import code.pages.theme
import net.liftweb.http.js.JsCmd
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait BWAModals extends BWAButtons with BWAEditor {

  trait SaveCancelModal extends theme.SaveCancelModal {

    type Size = String
    val SIZE_NORMAL: Size = ""
    val SIZE_BIG: Size = "big"
    val SIZE_HUGE: Size = "huge"
    def modalSize: Size = SIZE_NORMAL

    def cancelBtn: BtnClas = Btn.Default
    def saveBtn: BtnClas = Btn.Default

    override def templateTransforms: NodeSeq => NodeSeq = ".modal [class+]" #> modalSize andThen super.templateTransforms

    @Deprecated override def defaultBtnClass = saveBtn.cls
    @Deprecated override def defaultCancelBtnClass = cancelBtn.cls
  }

  trait Modal extends SaveCancelModal {
    override def templateTransforms: NodeSeq => NodeSeq = ".mdl-cancel" #> ClearNodes andThen super.templateTransforms
  }

  trait SaveCancelModalEditor extends theme.SaveCancelModal with Editor {

    type Size = String
    val SIZE_NORMAL: Size = ""
    val SIZE_BIG: Size = "big"
    val SIZE_HUGE: Size = "huge"
    def modalSize: Size = SIZE_NORMAL

    override def n: String = "saveCancelModalEditor"

    override def saved(): JsCmd = super.saved()

    def cancelBtn: BtnRenderable = Btn.Default.lbl("Cancel")
    def saveBtn: BtnRenderable = Btn.Success.lbl("Save").onclick(submitForm())

    def showCancelBtn: Boolean = true
    def showSaveBtn: Boolean = true

    override def templateTransforms: NodeSeq => NodeSeq =
      super.templateTransforms andThen
        ".mdl-cancel" #> (if (showCancelBtn) cancelBtn.rendered else NodeSeq.Empty) &
          ".mdl-save" #> (if (showSaveBtn) saveBtn.rendered else NodeSeq.Empty) &
          ".modal [class+]" #> modalSize

    override def contents: NodeSeq = renderedNoBtns
  }

}