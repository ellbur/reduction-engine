
scalaVersion := "2.10.1-RC3"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq(
    "-feature"
)

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("reductionengine.gui.AppletMain")

