import httpz.Action
import client.Command
import scalaz.{Free, ~>}

package object client {
  type CommandToAction = Command ~> Action

  type Commands[A] = Free[Command, A]

  val interpreter: CommandToAction = Interpreter
}
