package sm

import sun.plugin.dom.exception.InvalidStateException

class Parent
case object Dad extends Parent
case object Mom extends Parent

class BabyState
case object Asleep extends BabyState
case object Crying extends BabyState
case object Happy extends BabyState
case object Hungry extends BabyState

case class Baby(var state: BabyState)

trait Food
case object Milk extends Food

object BabyStateMachine extends StateMachine[Baby, Parent] {
  // matchers
  def is(expected: Parent): FromMatcher = Matcher({
    case (_, p) if p == expected => true
    case _ => false
  })
  val isMom: FromMatcher = is(Mom)
  val isDad: FromMatcher = is(Dad)

  def is(expected: BabyState): FromMatcher = Matcher({
    case (Baby(s), _) if s == expected => true
    case _ => false
  })
  val isHungry: FromMatcher = is(Hungry)
  val isCrying: FromMatcher = is(Crying)
  val isAsleep: FromMatcher = is(Asleep)
  val isHappy: FromMatcher = is(Happy)

  case object Feed extends Action with Payload[Food] {
    override val from: FromMatcher = isMom & isHungry
    override val to: ToMatcher = Matcher((baby: Baby) => baby.state == Happy)

    def run: Food => Transition = {
      case Milk  => (baby, _) => baby.state = Happy
      case _ => throw new InvalidStateException("baby threw up the food")
    }
  }

  case object Rock extends Action with NoPayload {
    override val from: FromMatcher = isDad & isCrying
    override def run:  Transition = (baby, _) => baby.state = Happy
  }

  override val actions: List[Action] = Nil
  override def create(bean: Baby, user: Parent): Unit = () => ()
}
