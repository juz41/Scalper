package network

trait GameIO {
  def broadcast(msg: String): Unit
  def send(clientId: String, msg: String): Unit
  def ask(clientId: String, prompt: String): String
}