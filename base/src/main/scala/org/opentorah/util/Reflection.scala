package org.opentorah.util

import java.lang.reflect.{Constructor, Field, Method}

object Reflection {

  type Argument = (Class[_], AnyRef)

  type Arguments = Seq[Argument]

  def parameterTypes(arguments: Arguments): Seq[Class[_]] = arguments.map(_._1)

  def args(arguments: Arguments): Seq[AnyRef] = arguments.map(_._2)

  def newInstance[T](clazz: Class[T], arguments: Argument*): T = {
    val constructor: Constructor[T] = clazz.getDeclaredConstructor(parameterTypes(arguments): _*)
    constructor.setAccessible(true)
    constructor.newInstance(args(arguments): _*)
  }

  def invoke[T](obj: AnyRef, methodName: String, arguments: Argument*): T = {
    val method: Method = obj.getClass.getDeclaredMethod(methodName, parameterTypes(arguments): _*)
    method.setAccessible(true)
    method.invoke(obj, args(arguments): _*).asInstanceOf[T]
  }

  def set(obj: AnyRef, fieldName: String, value: AnyRef): Unit = {
    val field: Field = obj.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.set(obj, value)
  }
}
