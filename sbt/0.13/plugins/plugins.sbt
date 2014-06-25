resolvers ++= Seq(
  "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.3.0")

addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.1.5")
