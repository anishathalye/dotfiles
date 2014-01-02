// Global build settings

// Imports

import scalariform.formatter.preferences._

// Scalariform - source code formatter

// disable automatic reformat on compile
// instead use 'scalariform-format' command in sbt
defaultScalariformSettings // instead of scalariformSettings

// list of preferences available at https://github.com/sbt/sbt-scalariform
ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
