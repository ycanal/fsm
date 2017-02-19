package sm

import sm.BabyStateMachine._

object SMApp extends App {
  val Alice = Baby(Crying)
  val Bob = Baby(Hungry)

  BabyStateMachine.update(Alice, Dad, Rock())
  BabyStateMachine.update(bean = Bob, user = Mom, transition = Feed(Milk))
}
