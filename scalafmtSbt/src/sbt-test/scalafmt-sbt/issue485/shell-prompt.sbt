shellPrompt.in(ThisBuild) := { state =>
  val project = Project.extract(state).currentRef.project
  s"[$project]> "
}
