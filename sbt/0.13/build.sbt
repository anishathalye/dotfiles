// Imports

import scalariform.formatter.preferences._

import de.johoop.findbugs4sbt.FindBugs._

import de.johoop.jacoco4sbt._

import JacocoPlugin._

// Global build settings

// Scalariform - source code formatter

// disable automatic reformat on compile
// instead use 'scalariform-format' command in sbt
defaultScalariformSettings // instead of scalariformSettings

// list of preferences available at http://mdr.github.io/scalariform/
ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)

// FindBugs settings

findbugsSettings

findbugsReportType := Some(de.johoop.findbugs4sbt.ReportType.FancyHistHtml)

findbugsPriority := de.johoop.findbugs4sbt.Priority.Low

findbugsEffort := de.johoop.findbugs4sbt.Effort.Maximum

findbugsReportPath := Some(crossTarget.value / "findbugs" / "report.html")

// JacocoPlugin settings

jacoco.settings
