package com.github.ivankliuk.concurrentprogramming

import scala.collection.mutable.ListBuffer


object Chapter2 {

  def startThread(a: => Unit): Thread = {
    val thread = new Thread {
      override def run(): Unit = a
    }
    thread.start()
    thread
  }

  /**
    * 1. Implement a [[parallel]] method, which takes two computation blocks,
    * `a` and `b`, and starts each of them in a new thread. The method must
    * return a tuple with the result values of both the computations. It should
    * have the following signature:
    * {{{
    * def parallel[A, B](a: => A, b: => B): (A, B)
    * }}}
    */

  def parallel[A, B](a: => A, b: => B): (A, B) = {
    class ThreadWrapper[C](computation: => C) {
      private var result: C = _

      def getResult: C = result

      def spawnThread: Thread = startThread {
        result = computation
      }
    }

    val tw1 = new ThreadWrapper(a)
    val tw2 = new ThreadWrapper(b)
    tw1.spawnThread.join()
    tw2.spawnThread.join()
    (tw1.getResult, tw2.getResult)
  }

  /**
    * 2. Implement a [[periodically]] method, which takes a time interval
    * `duration` specified in milliseconds, and a computation block `b`.
    * The method starts a thread that executes the computation block `b` every
    * `duration` milliseconds. It should have the following signature:
    * {{{
    * def periodically(duration: Long)(b: => Unit): Unit
    * }}}
    */

  // I had to add `break` condition into the signature to make the code testable.
  def periodically(duration: Long, break: => Boolean)(b: => Unit): Unit =
    if (!break) {
      startThread(b)
      Thread.sleep(duration)
      periodically(duration, break)(b)
    }
    else ()


  /**
    * 3. Implement a [[SyncVar]] class with the following interface:
    * {{{
    * class SyncVar[T] {
    *   def get(): T = ???
    *   def put(x: T): Unit = ???
    * }
    * }}}
    *
    * A [[SyncVar]] object is used to exchange values between two or more threads.
    * When created, the [[SyncVar]] object is empty:
    *  - Calling [[get]] throws an exception
    *  - Calling [[put]] adds a value to the [[SyncVar]] object
    * After a value is added to a [[SyncVar]] object, we say that it is non-empty:
    *  - Calling [[get]] returns the current value, and changes the state to empty
    *  - Calling [[put]] throws an exception
    */
  class SyncVar[T] {
    protected var state: Option[T] = None

    def get(): T = if (state.nonEmpty) {
      val returnValue = state.get
      state = None
      returnValue
    }
    else
      throw new Exception

    def put(x: T): Unit = if (state.isEmpty) {
      state = Some(x)
    }
    else
      throw new Exception
  }

  /**
    * 4. The [[SyncVar]] object from the previous exercise can be cumbersome to
    * use, due to exceptions when the [[SyncVar]] object is in an invalid state.
    * Implement a pair of methods, [[isEmpty]] and [[nonEmpty]], on the
    * [[SyncVar]] object. Then, implement a producer thread that transfers
    * a range of numbers 0 until 15 to the consumer thread that prints them.
    */
  class SyncVarFlagged[T] extends SyncVar[T] {
    def isEmpty: Boolean = state.isEmpty

    def nonEmpty: Boolean = state.nonEmpty
  }

  class SyncVarFlaggedInt extends SyncVarFlagged[Int] {
    val range: Range = 0 to 15

    // Instead of catching side effects, consumer writes values to the buffer.
    val resultBuffer = new ListBuffer[Int]

    val producer = startThread {
      range.foreach { element =>
        this.synchronized {
          if (this.isEmpty) {
            this.put(element)
          }
          else {
            this.notify()
          }
        }
      }
    }

    val consumer = startThread {
      while (resultBuffer.size != 16) {
        this.synchronized {
          if (this.nonEmpty) {
            resultBuffer += this.get()
          } else {
            this.notify()
          }
        }
      }
    }

  }

  /**
    * 5. Using the `isEmpty` and `nonEmpty` pair of methods from the previous
    * exercise requires busy-waiting. Add the following methods to the
    * [[SyncVar]] class:
    * {{{
    * def getWait(): T
    * def putWait(x: T): Unit
    * }}}
    * These methods have similar semantics as before, but go into the waiting
    * state instead of throwing an exception, and return once the [[SyncVar]]
    * object is empty or non-empty, respectively.
    */
  class SyncVarWait[T] extends SyncVarFlagged[T] {
    def getWait: T = synchronized {
      while (state.isEmpty) {
        this.wait()
      }
      state.get
    }

    def putWait(x: T): Unit = synchronized {
      while (state.nonEmpty) {
        this.wait()
      }
      state = Option(x)
      this.notify()
    }
  }

  class SyncVarWaitInt extends SyncVarWait[Int] {
    val range: Range = 0 to 15

    // Instead of catching side effects, consumer writes values to the buffer.
    val resultBuffer = new ListBuffer[Int]

    val producer = startThread {
      range.foreach {
        element => this.putWait(element)
      }
    }

    val consumer = startThread {
      while (resultBuffer.size != 16) {
        resultBuffer += this.getWait
      }
    }

  }

}