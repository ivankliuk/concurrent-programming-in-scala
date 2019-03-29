package com.github.ivankliuk.concurrentprogramming

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Chapter2Test extends FunSuite {

  import Chapter2._

  private val intValue = 10
  private val moment = 100

  test("`parallel` returns correct result") {
    parallel(1 + 2, "a" + "b") shouldEqual(3, "ab")
  }

  test("`periodically` updates a counter within period of time") {
    val duration = 20
    val iterations = 10
    var counter: Int = 0
    val startTime = System.currentTimeMillis
    periodically(duration, counter == iterations) {
      counter = counter + 1
    }
    val stopTime = System.currentTimeMillis
    val elapsedTime = stopTime - startTime

    (elapsedTime - (duration * iterations) < 50) shouldBe true
  }

  test("`SyncVar.get` returns value if not empty") {
    val syncVar = new SyncVar[Int]
    val thread = startThread {
      Thread.sleep(moment) // Imitates heavy computation.
      syncVar.put(intValue)
    }

    // Without this statement the exception below won't be thrown
    // due to race condition.
    thread.join()

    syncVar.get() shouldEqual intValue
  }

  test("`SyncVar.get` throws an exception if empty") {
    val syncVar = new SyncVar[Int]
    an[Exception] should be thrownBy {
      syncVar.get()
    }
  }

  test("`SyncVar.put` throws an exception if not empty") {
    val syncVar = new SyncVar[Int]
    val thread = startThread {
      Thread.sleep(moment) // Imitates heavy computation.
      syncVar.put(intValue)
    }

    // Without this statement the exception below won't be thrown
    // due to race condition.
    thread.join()

    an[Exception] should be thrownBy {
      syncVar.put(intValue)
    }
  }

  test("`SyncVarFlaggedInt` stores numbers correctly") {
    val syncVar = new SyncVarFlaggedInt
    syncVar.producer.join()
    syncVar.consumer.join()

    syncVar.resultBuffer.toList should equal(syncVar.range.toList)
  }

  test("`SyncVarWaitInt` stores numbers correctly") {
    val syncVar = new SyncVarWaitInt
    syncVar.producer.join()
    syncVar.consumer.join()

    syncVar.resultBuffer.toList should equal(syncVar.range.toList)
  }

}