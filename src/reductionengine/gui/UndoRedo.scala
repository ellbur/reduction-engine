
package reductionengine.gui

import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import reactive.Var

trait UndoRedo { self: Editor =>
  private var quiet: Boolean = false

  lazy val undoRedoModel = new DefaultListModel
  lazy val undoRedoThings = {
    val it = new JList(undoRedoModel)
    it.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

    it.addListSelectionListener(new ListSelectionListener {
      def valueChanged(ev: ListSelectionEvent) {
        if (!quiet) {
          val which = it.getSelectedIndex
          if (0 <= which && which < undoRedoModel.size) {
            undoRedoModel.get(which).asInstanceOf[UndoRedoPoint].goTo()
          }
        }
      }
    })

    val pane = new JScrollPane(it)
    pane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)

    (it, pane)
  }
  lazy val undoRedoSidebar = undoRedoThings._2
  lazy val undoRedoList = undoRedoThings._1

  case class UndoRedoPoint(description: String, state: FrozenEditingState) {
    def goTo() {
      softEdit.now foreach { description =>
        saveUndoRedoPoint(description)
      }
      state.goTo()
    }
    override def toString = description
  }

  val softEdit = Var[Option[String]](None)

  def actSoft(description: String) {
    softEdit() = Some(description)
  }

  def actHard(description: String) {
    saveUndoRedoPoint(description)
  }

  def saveUndoRedoPoint(description: String) {
    softEdit() = None
    val point = UndoRedoPoint(description, editingState.freeze.now)
    quiet = true
    undoRedoModel.add(0, point)
    undoRedoList.setSelectedIndex(0)
    quiet = false
  }

  saveUndoRedoPoint("--- Beginning ---")

  // For editing
  def undo() {
    val i = undoRedoList.getSelectedIndex
    if (i < undoRedoModel.size()-1) {
      undoRedoList.setSelectedIndex(i + 1)
    }
    else
      message("Nothing more to undo")
  }

  def redo() {
    val i = undoRedoList.getSelectedIndex
    if (i > 0) {
      undoRedoList.setSelectedIndex(i - 1)
    }
    else
      message("Nothing more to redo")
  }
}
