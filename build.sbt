name := "typed.funops.co"

organizationName := "Referential Labs"

description := "Blog about typed FP in building reliable infrastructures."

startYear := Option(2015)

homepage := Option(url("http://typed.funops.co/"))

organizationHomepage := Option(url("http://referentiallabs.com"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/typedops/funops"),
    "https://github.com/typedops/funops.git"
  )
)

scalaVersion := "2.11.8"

scalapropsSettings

scalapropsWithScalazlaws

scalapropsVersion := "0.3.2"

tutSettings

enablePlugins(JekyllPlugin)

tutSourceDirectory := sourceDirectory.value / "main" / "tut"

tutTargetDirectory := sourceDirectory.value / "jekyll" / "_posts"

ghpages.settings

git.remoteRepo := "git@github.com:typedops/funops.git"

libraryDependencies ++= Seq(
  // why use types post
  "org.spire-math"        %% "cats"                   % "0.2.0",
  "org.spire-math"        %% "spire"                  % "0.10.1",
  // bottom post
  "org.scalaz"            %% "scalaz-core"            % "7.2.1",
  "org.scalaz"            %% "scalaz-effect"          % "7.2.1"
)
