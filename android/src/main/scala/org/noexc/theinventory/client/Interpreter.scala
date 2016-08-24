package client

private[client] object Interpreter extends CommandToAction {
  override def apply[A](fa: Command[A]) = fa match {
    case c => c.action
  }
}
