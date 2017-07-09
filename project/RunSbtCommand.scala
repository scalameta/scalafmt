import sbt._
import Keys._
import sbt.complete.Parser

// Helper to execute command from tasks.
// Note: This was copied from https://github.com/sbt/sbt-release/blob/663cfd426361484228a21a1244b2e6b0f7656bdf/src/main/scala/ReleasePlugin.scala#L99-L115
object RunSbtCommand {
  def apply(command: String): State => State = { st: State =>
    @annotation.tailrec
    def runCommand(command: String, state: State): State = {
      val nextState = Parser.parse(command, state.combinedParser) match {
        case Right(cmd) => cmd()
        case Left(msg) => throw sys.error(s"Invalid programmatic input:\n$msg")
      }
      nextState.remainingCommands.toList match {
        case Nil => nextState
        case head :: tail =>
          runCommand(head, nextState.copy(remainingCommands = tail))
      }
    }
    runCommand(
      command,
      st.copy(
        remainingCommands = Nil,
        onFailure = Some(s"Failed to run $command")))
      .copy(remainingCommands = st.remainingCommands)
  }
}
