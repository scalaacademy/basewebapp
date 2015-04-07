package code.pages.theme

import com.github.david04.liftutils.elem.DefaultBS2HTMLEditor
import com.github.david04.liftutils.loc.LocC


trait BWAEditor {

  trait Editor extends DefaultBS2HTMLEditor with LocC {
    override protected def n: String = "editor"
  }

}
