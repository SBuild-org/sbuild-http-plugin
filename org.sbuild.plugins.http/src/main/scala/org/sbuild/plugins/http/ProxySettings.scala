package org.sbuild.plugins.http

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.sbuild.Project

sealed trait ProxySettings

object ProxySettings {
  private[this] val HttpProxy = "(http://)?([^:]+):([0-9]+)".r

  implicit def fromString(proxyAsString: String): ProxySettings = proxyAsString match {
    case null | "" => None
    case HttpProxy(_, host, port) if Try(port.toInt).isSuccess => Http(host = host, port = port.toInt)
    case p =>
      throw new IllegalArgumentException(s"Unsupported proxy definition: '${p}'. Supported format: [http://]<host>:<port>")
  }

  case object None extends ProxySettings
  case class Http(host: String, port: Int) extends ProxySettings
  case object AutoProxy extends ProxySettings {
    def proxySettings(implicit project: Project): ProxySettings =
      Try(ProxySettings.fromString(System.getenv("http_proxy"))) match {
        case Success(p) => p
        case Failure(e) =>
          project.monitor.warn(s"The content of the 'http_proxy' environment variable is not supported: '${System.getenv("http_proxy")}'")
          None
      }
  }

}