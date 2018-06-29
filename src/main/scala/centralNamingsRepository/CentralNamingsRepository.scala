package centralNamingsRepository

import java.net.SocketTimeoutException

import googleSpreadsheets._

import scala.util.{Failure, Try}

case class CentralNamingsRepository(globalNamings: Seq[GlobalNamingsRow]) {

  def globalNameSet: Set[String] = {
    globalNamings.map(_.globalNamingField).toSet
  }

}

object CentralNamingsRepository {

  def apply(): Try[CentralNamingsRepository] = {
    GoogleSpreadsheet(centralNamingsRepositoryGSId).flatMap(x => x.get(GlobalNamingsRow).map(new CentralNamingsRepository(_)))
  }


  private val centralNamingsRepositoryGSId = "1VKV_5Q-GeVwXLwG8Q7_6MNAapZDkWHTFApTO_72RUog"

}
