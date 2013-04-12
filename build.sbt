
scalaVersion := "2.10.1-RC3"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq(
    "-feature"
)

javacOptions ++= Seq(
    "-source", "-1.6", "-target", "-1.6"
)

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("reductionengine.gui.AppletMain")

libraryDependencies ++= Seq(
    "com.miglayout" % "miglayout-core" % "4.2",
    "com.miglayout" % "miglayout-swing" % "4.2",
    "cc.co.scala-reactive" %% "reactive-core" % "0.3.0",
    "com.github.ellbur" %% "reactive-map" % "0.1-SNAPSHOT",
    "org.scalaz" %% "scalaz-core" % "6.0.4"
)

