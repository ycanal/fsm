package sm

import sm.BabyStateMachine._

object SMApp extends App {
  val Alice = Baby(Crying)
  val Bob = Baby(Hungry)

  println("Before rocking: " + BabyStateMachine.availableActions(Alice, Dad))
  BabyStateMachine.update(Alice, Dad, Rock())
  println("After rocking: " + BabyStateMachine.availableActions(Alice, Dad))

  BabyStateMachine.update(bean = Bob, user = Mom, transition = Feed(Milk))

}
