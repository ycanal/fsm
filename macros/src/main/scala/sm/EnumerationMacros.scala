package sm

import scala.collection.immutable.HashSet
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object EnumerationMacros {
  def sealedInstancesOf[A]: HashSet[A] = macro sealedInstancesOf_impl[A]

  def sealedInstancesOf_impl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[HashSet[A]] = {
    import c.universe._

    val symbol = weakTypeOf[A].typeSymbol.asClass

    if  (!symbol.isClass || !symbol.isSealed)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")
    else {

      val children = symbol.knownDirectSubclasses.toList

      if (!children.forall(_.isModuleClass)) c.abort(c.enclosingPosition, "All children must be objects.")
      else c.Expr[HashSet[A]] {

        def sourceModuleRef(sym: Symbol) = Ident(sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol
          ].sourceModule.asInstanceOf[Symbol]
        )

        //noinspection ConvertibleToMethodValue
        Apply(
          Select(
            reify(HashSet).tree,
            TermName("apply")
          ),
          children.map(sourceModuleRef(_))
        )
      }
    }
  }
}
