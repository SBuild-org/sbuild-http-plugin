package org.sbuild.plugins.http

import java.io.File

case class Http(schemeName: String,
                downloadDir: File,
                proxy: ProxySettings)
