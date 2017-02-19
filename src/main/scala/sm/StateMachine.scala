package sm

abstract class StateMachine[T, U] {
  type StateMatcher = Matcher[T, U]
  type Transition = (T, U) => Unit
  private type ExecutableAction = (StateMatcher, StateMatcher, Transition)

  class Action {
    val from: StateMatcher = Matcher((_: T, _: U) => true)
    val to: StateMatcher = Matcher((_: T, _: U) => true)
  }
  trait NoPayload extends Action {
    def run: Transition
    def apply(): ExecutableAction = (from, to, run)
  }
  trait Payload[P] extends Action {
    def run: (P) => Transition
    def apply(payload: P): ExecutableAction = (from, to, run(payload))
  }

  val actions: List[Action]

  def create(bean: T, user: U): Unit
  def update[P](bean: T, user: U, transition: ExecutableAction): Unit = {
    val (from, to, run) = transition

    if (!from(bean, user)) {
      throw new IllegalStateException("action cannot be applied")
    }

    run(bean, user)

    if (!to(bean, user)) {
      throw new IllegalStateException("action applied but end state is fucked up")
    }
  }

  def availableActions(bean: T, user: U): List[Action] = {
    this.actions.filter(_.from(bean, user))
  }
}
