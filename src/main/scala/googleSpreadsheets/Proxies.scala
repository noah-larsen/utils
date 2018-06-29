package googleSpreadsheets

import scala.util.Try


/**
  * Skynet supports the use of proxies specified as environmental variables of the form HOSTNAME:PORT. http_proxy
  * specifies a proxy server, and http_proxy_grupobbva specifies a proxy server to use specifically with grupobbva
  * hostnames.
  */
object Proxies {

  val proxy: Option[Proxy] = proxyAddress.flatMap(proxy(_))
  val grupobbvaProxy: Option[Proxy] = grupobbvaProxyAddress.flatMap(proxy(_)).orElse(proxy)


  private def proxyAddress: Option[String] = {
    val proxyEnvVariable = "http_proxy"
    sys.env.get(proxyEnvVariable)
  }


  private def grupobbvaProxyAddress: Option[String] = {
    val grupobbvaProxyEnvVariable = "http_proxy_grupobbva"
    sys.env.get(grupobbvaProxyEnvVariable)
  }


  private def proxy(proxy: String): Option[Proxy] = {
    val hostnamePortNumberSeparator = ":"
    if (proxy.contains(hostnamePortNumberSeparator)) {
      val separatorIndex = proxy.indexOf(hostnamePortNumberSeparator)
      Try(Proxy(proxy.substring(0, separatorIndex), proxy.substring(separatorIndex + 1, proxy.length).toInt)).toOption
    }
    else {
      val defaultProxyPort = 8080
      Some(Proxy(proxy, defaultProxyPort))
    }
  }


  private[/*network*/googleSpreadsheets] case class Proxy(hostname: String, port: Int)

}
