/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 03/07/2013
 */

trait AsStack[T, CC <: AsStack[T, CC]] {
  def push(a: T): CC
  def pop: T
  def peek: T
  def size: Int
  def isEmpty: Boolean = size == 0
}

/*
 EStack is mutable, because 'pop' doesn't return both an element and an
 new stack.
 */
class EStack[T] extends AsStack[T, EStack[T]]
{
  // def this() = this(0)
  var last: AsNode[T] = NuNode
  var size = 0
  def push(a: T): EStack[T] = {
    try {
      last = new Node[T](a, last)
    } catch {
      // sIXME:
      case e: Any =>
        Console.print("[Err] new Node")
        throw e
    }
    size = size + 1
    this
  }

  def pop: T = {
    if (isEmpty)
      throw new IllegalAccessException
    else {
      val value = last match {
        case Node(v, _) => v
      }
      last = last.prev
      size = size - 1
      value
    }
  }

  def peek: T = {
    if (isEmpty)
      throw new IllegalAccessException
    else {
      val value = last match {
        case Node(v, _) => v
      }
      value
    }
  }
}


