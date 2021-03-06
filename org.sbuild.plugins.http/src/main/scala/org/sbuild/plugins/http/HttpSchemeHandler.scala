package org.sbuild.plugins.http

import java.io.File
import java.io.FileNotFoundException
import java.net.URL
import org.sbuild.internal.I18n
import org.sbuild.Path
import org.sbuild.SchemeResolver
import org.sbuild.SchemeHandler
import org.sbuild.SchemeHandler.SchemeContext
import org.sbuild.TargetContext
import org.sbuild.CmdlineMonitor
import org.sbuild.SBuildVersion
import org.sbuild.Project
import org.sbuild.Logger

/**
 * An HTTP-Scheme handler, that will download the given URI into a directory preserving the URI as path.
 * Example:
 * The HttpSchemeHandler is configured to use '.sbuild/http' as download directory
 * The file 'http://example.com/downloads/example.jar' will be downloaded into
 * '.sbuild/http/example.com/downloads/example.jar'
 */
class HttpSchemeHandler(downloadDir: File = null,
                        forceDownload: Boolean = false,
                        proxySettings: ProxySettings = ProxySettings.AutoProxy)(implicit project: Project) extends HttpSchemeHandlerBase(
  Option(downloadDir).getOrElse(Path(".sbuild/http")),
  forceDownload)
    with SchemeResolver {

  Logger[HttpSchemeHandler].debug("Created " + this)

  override def resolve(schemeCtx: SchemeContext, targetContext: TargetContext) = {
    val lastModified = download(schemeCtx.path, project.monitor)
    targetContext.targetLastModified = lastModified
  }

  override def toString() = super.toStringBase("project=" + project.projectFile)

}

class HttpSchemeHandlerBase(val downloadDir: File, val forceDownload: Boolean = false) extends SchemeHandler {

  var online: Boolean = true

  private val userAgent = s"SBuild/${SBuildVersion.osgiVersion} (HttpSchemeHandler)"

  def url(path: String): URL = new URL("http:" + path)

  override def localPath(schemeCtx: SchemeContext): String = "file:" + localFile(schemeCtx.path).getPath

  def localFile(path: String): File = {
    url(path)
    // ok, path is a valid URL
    new File(downloadDir, path)
  }

  /**
   * @return The last modified time stamp of the file.
   */
  def download(path: String, monitor: CmdlineMonitor): Long = {
    val target = localFile(path)
    if (online) {
      if (!forceDownload && target.exists) {
        target.lastModified
      } else {
        val url = this.url(path)
        //        println("Downloading " + url + "...")
        HttpSupport.download(url.toString, target.getPath, monitor, Some(userAgent)) match {
          case Some(e) => throw e
          case _ => target.lastModified
        }
      }
    } else {
      if (target.exists) {
        target.lastModified
      } else {
        val msg = I18n.marktr("File is not present and can not be downloaded in offline-mode: {0}")
        throw new FileNotFoundException(I18n.notr(msg, target.getPath)) {
          override def getLocalizedMessage: String = I18n[HttpSchemeHandlerBase].tr(msg, target.getPath)
        }
      }
    }
  }

  def toStringBase(extra: String = "") = getClass.getSimpleName +
    "(downloadDir=" + downloadDir +
    ",forceDownload=" + forceDownload +
    ",online=" + online +
    "," + extra +
    ")"

  override def toString() = toStringBase()
}
