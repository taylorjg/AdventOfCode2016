val commonSettings = Seq(
  scalaVersion := "2.12.0"
)

val commonDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val day1 = project.in(file("Day1"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day2 = project.in(file("Day2"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day3 = project.in(file("Day3"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day4 = project.in(file("Day4"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day5 = project.in(file("Day5"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day6 = project.in(file("Day6"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day7 = project.in(file("Day7"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val day8 = project.in(file("Day8"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)
