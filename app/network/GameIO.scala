package network

trait GameIO {
  def broadcast(msg: ServerToClient): Unit
  def send(clientId: String, msg: ServerToClient): Unit
  def ask(clientId: String, promptInfo: ServerToClient): ClientToServer
}