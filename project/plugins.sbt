resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.0.0")
addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.1.1")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4" exclude("com.typesafe.sbt", "sbt-git"))
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.4.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
