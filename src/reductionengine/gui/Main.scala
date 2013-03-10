
package reductionengine.gui

import javax.swing.JFrame

object Main extends scala.App {
  val editor = new Editor

  val win = new JFrame()
  win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  win.add(editor)

  win.setSize(800, 600)
  win.setVisible(true)
}
