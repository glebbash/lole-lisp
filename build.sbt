val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lole-lisp",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6"
    // resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    // libraryDependencies += "org.bytedeco" % "llvm-platform" % "13.0.0-1.5.7-SNAPSHOT"
    // libraryDependencies += "org.bytedeco" % "llvm-platform" % "13.0.0-1.5.7-SNAPSHOT" from "file://workspaces/lole-lisp/lib/llvm-13.0.0-1.5.7-20220117.160352-82.jar"
  )
