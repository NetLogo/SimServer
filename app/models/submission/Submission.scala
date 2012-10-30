package models.submission

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 2:00 PM
 */

sealed trait Submission {
  def id: Option[Long]
}

trait Association extends Submission {
  def refID : Option[Long]
}

trait Entry extends Submission

//@ Umm... so let me get this straight, Mr. Jason...
//  It _is_ alright for something to know that it's presentable
//  But it's _not_ alright for something to actually know _how_ to present itself?
//  ...THAT DON'T MAKE NO SENSE!!!!!!  Just give this trait an abstract `present` method and be done with it!!! --JAB (10/30/12)
trait Presentable {
  def typ: String
}