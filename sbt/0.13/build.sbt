// Imports

import scalariform.formatter.preferences._

// Global build settings

fork := true

// Scalariform - source code formatter

// disable automatic reformat on compile
// instead use 'scalariform-format' command in sbt
defaultScalariformSettings // instead of scalariformSettings

// list of preferences available at http://mdr.github.io/scalariform/
ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
