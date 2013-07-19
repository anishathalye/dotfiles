// Global build settings

// Imports

import scalariform.formatter.preferences._

// Scalariform - source code formatter

// disable automatic reformat on compile
// instead use 'scalariform-format' command in sbt
defaultScalariformSettings // instead of scalariformSettings

// list of preferences available at http://mdr.github.io/scalariform/
ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)

// Scalastyle - Scala style checker
org.scalastyle.sbt.ScalastylePlugin.Settings

// sbt-site - Project website generator
site.settings

site.includeScaladoc()

// site.pamfletSupport() // enable this in specific projects

// sbt-ghpages - GitHub pages uploader
// ghpages.settings

// git.remoteRepo := "git@github.com:{username}/{project}.git"
