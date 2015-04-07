//  Copyright (c) 2014 David Miguel Antunes <davidmiguel {at} antunes.net>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.

package com.github.david04.liftutils.crud

import com.github.david04.liftutils.datatables.Table
import com.github.david04.liftutils.elem._
import com.github.david04.liftutils.entity.{StdEntity, StdEntityBase}
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.NodeSeq

trait CrudC {
  def clas: this.type = this

  type CrudIType <: CrudI

  def crudName: String

  def getCrudI(id: Long): CrudIType

  trait CrudI {
    def id: Long

    def delete(): Unit

    def save(): Unit
  }

}

trait CrudChildC extends CrudC {

  type CrudParentCType <: CrudParentC
  type CrudIType <: CrudChildI

  def parent: CrudParentCType#CrudIType

  def all(): Seq[CrudIType]

  def create(): CrudIType

  trait CrudChildI extends CrudI {
    type CrudChildCType <: CrudChildC
  }

}

trait CrudParentC extends CrudC {
  type CrudIType <: CrudParentI

  def children: Seq[CrudC] = Seq()

  trait CrudParentI extends CrudI {

  }

}

trait NamedCrudC extends CrudC {
  def singular: String

  def plural: String
}


trait ListableC extends CrudChildC {
  type CrudIType <: ListableI
  type ViewableTypeC
  type ViewableTypeI

  def listViewableTypeC(): List[ViewableTypeC]

  trait ListableI extends CrudChildI {

    def viewableTypeI(clas: ViewableTypeC): ViewableTypeI
  }

}

trait HTMLListableC extends ListableC {
  def listTransform: NodeSeq => NodeSeq = PassThru
}

trait Editable {
  def editElems()(implicit editor: HTMLEditorImpl): List[HTMLEditableElem]
}

trait Creatable {
  def createElems()(implicit editor: HTMLEditorImpl): List[HTMLEditableElem]
}

trait DataTableListableC extends CrudChildC with HTMLListableC with NamedCrudC {
  type ViewableTypeC = String
  type ViewableTypeI = NodeSeqViewableElem
  //type CrudIType <: ListableI[this.type, ViewableTypeI]

  override def listTransform = {
    //    val table = new Table[CrudIType](() => all()) with T.cl {
    //      protected def plural: String = clas.plural
    //
    //      protected def singular: String = clas.singular
    //
    //      val columns = listViewableTypeC().map(clas =>
    //        new Col[CrudIType](clas.name, c => JString(c.viewableTypeI(clas).renderNodeSeqView.toString()))
    //      )
    //    }
    super.listTransform //andThen
    //      "@crud-table" #> table.render
  }
}


trait CrudUBase {

  def id: Long

  def crud: CrudBase

  def renderU: NodeSeq => NodeSeq
}

trait CrudCRDBase extends CrudBase {

  def renderCRD: NodeSeq => NodeSeq
}

trait CrudBase {

  def crudName: String = singleton.entityName

  def singleton: StdEntityBase

  def fromId(id: Long): Option[CrudUBase]
}

trait Crud[T <: StdEntity[T]] extends CrudBase {

  def singleton: T
}

trait CrudCRD[T <: StdEntity[T]] extends Crud[T] with CrudCRDBase {

  def listTable(): Table[T]

  def renderCRD = {
    val table = listTable()

    //    "@crud-create [onclick]" #> SHtml.onEvent(_ => new Modal {
    //      val editor = new Editor {
    //        val instance = singleton.create
    //
    //        def buildElems() = instance.elems(this)
    //
    //        protected def saved = {
    //          instance.save()
    //          table.reload() & hide()
    //        }
    //      }
    //      val body: NodeSeq = editor.renderElems(Templates("templates-hidden" :: "editor" :: Nil).get)
    //
    //      val title: String = "New " + singleton.singular.capitalize
    //      val buttons: NodeSeq = (
    //        "@editor-save" #> editor.renderSubmitBtn
    //        ).apply(Templates("templates-hidden" :: "editor-btns" :: Nil).get)
    //
    //      val icon: String = ""
    //    }.show()) &
    "@crud-table" #> table.render &
      "@crud-singular" #> singleton.singular.capitalize &
      "@crud-singular-lc" #> singleton.singular.toLowerCase &
      "@crud-plural" #> singleton.plural.capitalize &
      "@crud-plural-lc" #> singleton.plural.toLowerCase
  }
}

trait CrudU[T <: StdEntity[T]] extends CrudUBase {

  def instance: T

  def id: Long = instance.id

  def renderU = (ns: NodeSeq) => {
    //    Script(new Modal {
    //      val editor = new Editor {
    //        def buildElems() = instance.elems(this)
    //
    //        protected def saved = { instance.save(); Noop }
    //      }
    //      val body: NodeSeq = editor.renderElems(Templates("templates-hidden" :: "editor" :: Nil).get)
    //
    //      val title: String = "Edit " + crud.singleton.singular.capitalize
    //      val buttons: NodeSeq = (
    //        "@editor-save" #> editor.renderSubmitBtn
    //        ).apply(Templates("templates-hidden" :: "editor-btns" :: Nil).get)
    //
    //      val icon: String = ""
    //    }.show()) ++
    ("@crud-singular" #> crud.singleton.singular.capitalize &
      "@crud-singular-lc" #> crud.singleton.singular.toLowerCase &
      "@crud-plural" #> crud.singleton.plural.capitalize &
      "@crud-plural-lc" #> crud.singleton.plural.toLowerCase)(ns)
  }
}
