package models.crypto

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/21/13
 * Time: 3:44 PM
 */

sealed trait SizedKey {
  protected def keySize: Int
}

trait K2048 extends SizedKey {
  override protected val keySize = 2048
}

