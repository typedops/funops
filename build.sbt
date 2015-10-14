tutSettings

site.settings

site.jekyllSupport()

ghpages.settings

git.remoteRepo := "git@github.com:typedops/funops.git"

tutTargetDirectory := sourceDirectory.value / "jekyll" / "_posts"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.2.0",
  "org.spire-math" %% "spire" % "0.10.1"
)
