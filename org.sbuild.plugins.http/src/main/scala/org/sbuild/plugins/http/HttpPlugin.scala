package org.sbuild.plugins.http

import org.sbuild.Plugin
import org.sbuild.Project
import org.sbuild.SchemeHandler
import org.sbuild.Path

class HttpPlugin(implicit project: Project) extends Plugin[Http] {

  def create(name: String): Http = {
    val schemeName = if (name == "") "http" else name
    Http(
      schemeName = schemeName,
      downloadDir = Path(".sbuild/http"),
      proxy = ProxySettings.AutoProxy
    )
  }

  def applyToProject(instances: Seq[(String, Http)]): Unit = instances foreach {
    case (name, http) =>
      SchemeHandler(http.schemeName, new HttpSchemeHandler(downloadDir = http.downloadDir))
  }

}