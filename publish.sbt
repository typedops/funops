pomExtra :=
    <developers>
      <developer>
        <name>Susan Potter (twitter: @SusanPotter)</name>
        <email>me@susanpotter.net</email>
      </developer>
    </developers>

lazy val gpgFolder = sys.env.getOrElse("GPG_FOLDER", ".")

pgpPassphrase := Some(sys.env.getOrElse("GPG_PASSPHRASE", "").toCharArray)

pgpPublicRing := file(s"${gpgFolder}/pubring.gpg")

pgpSecretRing := file(s"${gpgFolder}/secring.gpg")

credentials += Credentials("Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  sys.env.getOrElse("SBT_PUBLISH_USERNAME", ""),
  sys.env.getOrElse("SBT_PUBLISH_PASSWORD", ""))

publishArtifact in Test := false
