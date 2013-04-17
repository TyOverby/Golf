package com.tyoverby.golf

import scala.collection.immutable.StringLike

class CaseIterator {
  private[this] var value = 0
  override def toString = {
    value += 1
    "Case #" + value +": "
  }
}
