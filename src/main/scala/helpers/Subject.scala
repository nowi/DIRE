package helpers

/**
 * User: nowi
 * Date: 06.02.2010
 * Time: 20:06:26
 */

trait Subject {
  type Observer = { def receiveUpdate(subject:Any) }

  private var observers = List[Observer]()
  def addObserver(observer:Observer) = observers ::= observer
  def notifyObservers(update : Any)  {
    observers foreach (_.receiveUpdate(update))
  }
}
