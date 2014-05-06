package org.sbuild.plugins.http

import java.io.FileInputStream
import org.sbuild.SBuildException
import java.net.URL
import java.text.DecimalFormat
import java.io.FileOutputStream
import java.io.FileNotFoundException
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import org.sbuild.CmdlineMonitor
import java.io.IOException
import java.io.File
import org.sbuild.NoopCmdlineMonitor
import java.net.Proxy
import java.net.InetSocketAddress
import org.sbuild.Project
import scala.util.Try
import org.sbuild.Logger

object HttpSupport {

  def download(url: String, target: String, monitor: CmdlineMonitor = NoopCmdlineMonitor, userAgent: Option[String], retryCount: Int = 5): Option[Throwable] = {

    try {

      val targetFile = new File(target)

      targetFile.exists match {
        case true => { // File already exists
          monitor.info(CmdlineMonitor.Verbose, "File '" + target + "' already downloaded")
          true
        }
        case false => { // File needs download

          monitor.info(CmdlineMonitor.Default, s"Downloading ${url}")

          // copy into temp file

          val dir = targetFile.getAbsoluteFile.getParentFile
          dir.mkdirs
          val downloadTargetFile = File.createTempFile(".~" + targetFile.getName, "", dir)

          def cleanup =
            if (downloadTargetFile.exists)
              if (!downloadTargetFile.delete)
                downloadTargetFile.deleteOnExit

          val outStream = new BufferedOutputStream(new FileOutputStream(downloadTargetFile))
          try {

            var lastContentLength: Option[Long] = None
            var len = 0
            var lastRetryLen = 0
            var retry = true
            var retries = 0

            while (retry) {
              retry = false

              val connection = new URL(url).openConnection
              userAgent.map { agent => connection.setRequestProperty("User-Agent", agent) }
              if (len > 0) {
                // TODO: also check http header Accept-Ranges
                connection.setRequestProperty("Range", s"bytes=${len}-")
              }
              val inStream = new BufferedInputStream(connection.getInputStream())

              // TODO: evaluate status code, e.g. 404

              // connection opened
              val contentLength = lastContentLength.getOrElse {
                val cl = connection.getHeaderField("content-length") match {
                  case null => -1
                  case length => try { length.toLong } catch { case _: Exception => -1 }
                }
                lastContentLength = Some(cl)
                cl
              }

              var last = System.currentTimeMillis
              var break = false
              var alreadyLogged = false
              val forceLogAfter = 5000
              val bufferSize = 1024

              val format = new DecimalFormat("#,##0.#")
              def formatLength(length: Long): String = format.format(length / 1024)

              def logProgress = if (contentLength > 0) {
                monitor.info(CmdlineMonitor.Default, s"Downloaded ${formatLength(len)} of ${formatLength(contentLength)} kb (${format.format((len.toDouble * 1000 / contentLength.toDouble).toLong.toDouble / 10)}%) from ${url}")
              } else {
                monitor.info(CmdlineMonitor.Default, s"Downloaded ${formatLength(len)} kb from ${url}")
              }

              var buffer = new Array[Byte](bufferSize)

              while (!break) {
                val now = System.currentTimeMillis
                if (len > 0 && now > last + forceLogAfter) {
                  alreadyLogged = true
                  logProgress
                  last = now;
                }
                inStream.read(buffer, 0, bufferSize) match {
                  case x if x < 0 => break = true
                  case count => {
                    len = len + count
                    outStream.write(buffer, 0, count)
                  }
                }
              }

              if (alreadyLogged && len > 0) {
                logProgress
              }

              // TODO: if no resume is supported, retry n times before giving up
              // TODO: implement a timeout, to avoid ever-blocking downloads

              contentLength match {
                case l if l < 0 => // cannot use contentLength to verify result
                case l if len == l => // download size is equal to expected size => good
                case l if len < l =>
                  // stream closed to early
                  monitor.info(CmdlineMonitor.Default, s"Download stream closed before download was complete from ${url}")

                  if (retries < retryCount) {
                    monitor.info(CmdlineMonitor.Default, s"Resuming download from ${url}")
                    retry = true
                    alreadyLogged = true
                    retries = if (len > lastRetryLen) 0 else (retries + 1)
                    lastRetryLen = len
                  } else {
                    outStream.close
                    cleanup
                    throw new SBuildException(s"To many failed retries (s${retries}). Cannot download from ${url}");
                  }
                case _ =>
                  outStream.close
                  cleanup
                  throw new SBuildException(s"Size of downloaded file does not match expected size: ${url}");
              }

              inStream.close

            }

          } catch {
            case e: FileNotFoundException =>
              outStream.close
              cleanup
              throw new SBuildException("Download resource does not exists: " + url, e);
            case e: IOException =>
              outStream.close
              cleanup
              throw new SBuildException("Error while downloading file: " + url, e);
          } finally {
            outStream.close
          }

          val renameSuccess = downloadTargetFile.renameTo(targetFile)
          if (!renameSuccess) {
            // move temp file to dest file
            val out = new FileOutputStream(targetFile)
            val in = new FileInputStream(downloadTargetFile)
            try {
              out.getChannel.transferFrom(in.getChannel, 0, Long.MaxValue)
            } finally {
              out.close
              in.close
            }
            cleanup
          }

        }
      }
      None
    } catch {
      case x: Throwable => Some(x)
    }

  }

}