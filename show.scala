import cats.Show
import shapeless3.deriving.*

object ShowImplicits:

  /** Scala 3 类型: [A] => (Show[A], A) => String
    *
    * 也就是泛型函数(Polymorphic Function), 与下面的函数签名等价:
    *
    * {{{
    * def showFunc[A](sa: Show[A], a: A): String = ???
    * }}}
    *
    * 这个泛型函数描述了在已知 ①数据的类型; ②该类型的 Show 实例; ③数据; 的情况下该如何显示这个数据
    */
  private val showFunc = [A] => (sa: Show[A], a: A) => sa.show(a)

  given showProduct[T](using instances: K0.ProductInstances[Show, T], labelling: Labelling[T]): Show[T] with
    def show(value: T): String =
      def showElem(elemLabel: String, elemIndex: Int): String =
        val elemShow = instances.project(value)(elemIndex)(showFunc)
        s"${elemLabel}=${elemShow}"

      val label = labelling.label

      if labelling.elemLabels.isEmpty then label
      else labelling.elemLabels.zipWithIndex.map(showElem).mkString(s"${label}(", ", ", ")")

  given showCoproduct[T](using instances: K0.CoproductInstances[Show, T]): Show[T] with
    def show(value: T): String = instances.fold(value)(showFunc)

  extension (show: Show.type)
    /** 定义为内联扩展函数:
      *   - 内联, 编译时搜索 shapeless 隐式参数, 展开生成函数的具体实现
      *   - 扩展函数, Scala 3 derives 语法要求 Type Class 的伴生对象定义有一个 derived 函数
      */
    inline def derived[T](using generic: K0.Generic[T]): Show[T] =
      generic.derive(showProduct, showCoproduct)

import ShowImplicits.*

case class Pet(name: String, age: Int) derives Show

@main
def run(): Unit =
  print(Show[Pet].show(Pet("tom", 8)))
