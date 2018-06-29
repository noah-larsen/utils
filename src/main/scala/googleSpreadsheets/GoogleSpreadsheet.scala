package googleSpreadsheets

import java.io.{File, FileNotFoundException}
import java.net.{InetSocketAddress, Proxy}
import java.text.Normalizer

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.GoogleUtils
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.sheets.v4.model.{AppendValuesResponse, UpdateValuesResponse, ValueRange}
import com.google.api.services.sheets.v4.{Sheets, SheetsScopes}
import googleSpreadsheets.GoogleSpreadsheet.ValueInputOptions

import scala.collection.JavaConverters._
import scala.io.{BufferedSource, Source}
import scala.util.Try

class GoogleSpreadsheet(sheets: Sheets, spreadsheetId: String) {

  def get(sheetRange: SheetRange): Try[Seq[Seq[String]]] = {
    Try(Option(sheets.spreadsheets.values.get(spreadsheetId, sheetRange.range).execute.getValues).map(_.asScala.map(_.asScala.map(_.toString).toSeq)).getOrElse(Seq()))
  }


  def get[T <: Row](dataReader: DataReader[T]): Try[Seq[T]] = {
    get(dataReader.sheetRange).map(_.map(dataReader.toRow))
  }


  def update[T <: Row](rows: Seq[T], dataReaderWriter: DataReaderWriter[T], fromRow: Option[Int] = None, toRow: Option[Int] = None, valueInputOption: ValueInputOptions.Value = ValueInputOptions.userEntered): Try[UpdateValuesResponse] = {

//    val range = Some(dataReaderWriter.sheetRange).map(x => x.copy(fromRow = fromRow.getOrElse(x.fromRow), toRow = toRow.orElse(x.toRow))).map(x => if(x.toRow.isEmpty) x.copy(fromRow = x.fromRow + get(dataReaderWriter).length) else x).get.range
    val range = Some(dataReaderWriter.sheetRange).map(x => x.copy(fromRow = fromRow.getOrElse(x.fromRow), toRow = toRow.orElse(x.toRow))).map(x => if(x.toRow.isEmpty) get(dataReaderWriter).map(y => x.copy(fromRow = x.fromRow + y.length)) else Try(x)).get.map(_.range)


//    Try(sheets.spreadsheets.values.update(spreadsheetId, range, new ValueRange().setValues(rows.map(dataReaderWriter.toStringSeq(_).map(_.asInstanceOf[AnyRef]).asJava).asJava)).setValueInputOption(valueInputOption.toString).execute())
    range.flatMap(x => Try(sheets.spreadsheets.values.update(spreadsheetId, x, new ValueRange().setValues(rows.map(dataReaderWriter.toStringSeq(_).map(_.asInstanceOf[AnyRef]).asJava).asJava)).setValueInputOption(valueInputOption.toString).execute()))

  }


  def append[T <: Row](rows: Seq[T], dataReaderWriter: DataReaderWriter[T], valueInputOption: ValueInputOptions.Value = ValueInputOptions.userEntered): Try[AppendValuesResponse] = {

//    val range = Some(dataReaderWriter.sheetRange).filter(_.toRow.isEmpty).map(x => x.copy(fromRow = x.fromRow + get(dataReaderWriter).length)).getOrElse(dataReaderWriter.sheetRange).range
    val range = Some(dataReaderWriter.sheetRange).filter(_.toRow.isEmpty).map(x => get(dataReaderWriter).map(y => x.copy(fromRow = x.fromRow + y.length))).getOrElse(Try(dataReaderWriter.sheetRange)).map(_.range)

    range.flatMap(x => Try(sheets.spreadsheets.values.append(spreadsheetId, x, new ValueRange().setValues(rows.map(dataReaderWriter.toStringSeq(_).map(_.asInstanceOf[AnyRef]).asJava).asJava)).setValueInputOption(valueInputOption.toString).execute()))
  }


  def rangesInclusive[T <: Row](dataReader: DataReader[T], condition: T => Boolean): Try[Seq[(Int, Int)]] = {

    def rangesInclusive(indexes: Iterable[Int]): Seq[(Int, Int)] = {
      Some(indexes.toSeq.distinct.sorted).map(x => x.headOption.map(y => x.sliding(2).filter(z => z.last - z.head != 1 && z.lengthCompare(1) != 0).flatten.toSeq.+:(y).:+(x.last).grouped(2).map(z => (z.head, z.last)).toSeq).getOrElse(Seq())).get
    }


    get(dataReader).map(x => rangesInclusive(x.zipWithIndex.map(y => (y._1, y._2 + dataReader.sheetRange.fromRow)).filter(y => condition(y._1)).map(_._2)))

  }

}

object GoogleSpreadsheet {

  //todo
  def apply(spreadsheetId: String, secretFile: String = "client_secret.json"): Try[GoogleSpreadsheet] = {
    authorize(secretFile).flatMap(sheets).map(new GoogleSpreadsheet(_, spreadsheetId))
  }


  object ValueInputOptions extends Enumeration {
    val raw: Value = Value("RAW")
    val userEntered: Value = Value("USER_ENTERED")
  }


  private val applicationName = "Datahub Automation"
  //todo
  private val dataStoreDir = new File(sys.props.get("skynet.gdrive.credentials.dir").getOrElse(sys.props.get("user.home").get),
    ".credentials/sheets.googleapis.skynet")
  private val dataStoreFactory = new FileDataStoreFactory(dataStoreDir)
  private val jsonFactory: JacksonFactory = JacksonFactory.getDefaultInstance
  private val httpTransport: NetHttpTransport = {
    val builder = new NetHttpTransport.Builder
    builder.trustCertificates(GoogleUtils.getCertificateTrustStore)
    Proxies.proxy.foreach(x => builder.setProxy(new Proxy(Proxy.Type.HTTP, new InetSocketAddress(x.hostname, x.port))))
    builder.build()
  }
  //todo
//  private val sheetScope = List(SheetsScopes.SPREADSHEETS_READONLY)
  private val sheetScope = List(SheetsScopes.SPREADSHEETS)


  private def authorize(secretFile: String): Try[Credential] = Try {

    def closer[T, C <: Source](c: C)(f: C => T): T = try f(c) finally c.close


    def perform(secret: BufferedSource) = {
      val clientSecrets = GoogleClientSecrets.load(jsonFactory, secret.reader())
      val flow = new GoogleAuthorizationCodeFlow.Builder(httpTransport, jsonFactory, clientSecrets, sheetScope.asJava)
        .setDataStoreFactory(dataStoreFactory).setAccessType("offline").build
      val credential = new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver).authorize("user")
      credential
    }


    val is = getClass.getClassLoader.getResourceAsStream(secretFile)
    if (is == None.orNull) throw new FileNotFoundException(s"Cannot find secret file $secretFile in the classpath.")
    closer(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(secretFile)))(perform)

  }


  private def sheets(credential: Credential): Try[Sheets] = Try {
    new Sheets.Builder(httpTransport, jsonFactory, credential).setApplicationName(applicationName).build
  }

}

