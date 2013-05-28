
package reductionengine.gui

import javax.swing.JPanel
import reductionengine._

class Editor extends JPanel
  with reactive.Observing
  with redosignals.Observing
  with Bubbles
  with KindsOfBubbles
  with OurBubbles
  with FocusAndGroups
  with Reductions
  with GarbageCollection
  with DiagramMotion
  with Editing
  with Burying
  with GeometricMotion
  with Layout
  with Rendering
  with Inputs
  with SomeExperiments
  with UndoRedo


