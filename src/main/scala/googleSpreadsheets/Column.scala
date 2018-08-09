package googleSpreadsheets

import utils.enumerated.SelfNamed

trait Column[T] extends SelfNamed {

  def string: T => String

}
