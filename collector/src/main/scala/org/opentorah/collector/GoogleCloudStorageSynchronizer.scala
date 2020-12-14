package org.opentorah.collector

import com.google.api.gax.paging.Page
import com.google.auth.oauth2.ServiceAccountCredentials
import com.google.cloud.WriteChannel
import com.google.cloud.storage.{Blob, BlobId, BlobInfo, Storage, StorageOptions}
import com.google.common.hash.Hashing
import org.opentorah.util.{Files, Strings}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.URLConnection
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

// documentation: https://github.com/googleapis/java-storage
final class GoogleCloudStorageSynchronizer private(
  serviceAccountKey: String,
  bucketName: String,
  bucketPrefix: String,
  directoryPath: String,
  dryRun: Boolean
) {
  require(directoryPath.endsWith("/"))

  private val logger: Logger = LoggerFactory.getLogger(classOf[GoogleCloudStorageSynchronizer])

  private def log(message: String): Unit = logger.info(message)

  private val credentials: ServiceAccountCredentials = ServiceAccountCredentials
    .fromStream(Strings.string2stream(serviceAccountKey))
    .asInstanceOf[ServiceAccountCredentials]

  private val storage: Storage = StorageOptions.newBuilder()
    .setCredentials(credentials)
    .build()
    .getService

  private def sync(): Unit = {
    log(s"Synchronizing $directoryPath to $bucketName/$bucketPrefix" + (if (dryRun) " (dry run)" else ""))

    log(s"Listing files in $directoryPath")
    val files: Map[String, File] = listDirectory(directoryPath)
    log(s"Found ${files.size} files")

    log(s"Listing blobs in $bucketName/$bucketPrefix")
    val blobs: Map[String, Blob] = listBucketDirectory(bucketPrefix)
    log(s"Found ${blobs.size} blobs")


    val toDelete: List[Blob] = blobs.values.toList
      .filter(blob => !files.contains(blob.getName))
      .sortBy(_.getName)
    log(s"Found ${toDelete.length} blobs to delete")

    val newFiles: List[(String, File)] = files.toList
      .filter { case (name, _) => !blobs.contains(name) }
      .sortBy { case (name, _) => name }

    val toCreateDirectories: List[String] = newFiles
      .filter { case (_, file) => file.isDirectory }
      .map    { case (name, _) => name }
    log(s"Found ${toCreateDirectories.length} directories to create")

    val toUpload: List[(String, File)] = newFiles
      .filter { case (_, file) => !file.isDirectory }
    log(s"Found ${toUpload.length} files to upload")

    val existing: List[(Blob, File)] = for {
      (name, blob) <- blobs.toList
      file <- files.get(name)
    } yield (blob, file)

    val flavourChanged: List[String] = existing
      .filter { case (blob, file) => isDirectory(blob) != file.isDirectory }
      .map    { case (blob, file) =>
        def status(isDirectory: Boolean): String = if (isDirectory) "is" else "is not "
        s"Blob $blob ${status(isDirectory(blob))} but file $file ${status(file.isDirectory)}"
      }

    if (flavourChanged.nonEmpty)
      throw new IllegalArgumentException("isDirectory changes: " + flavourChanged.mkString("\n"))

    val toUpdate: List[(Blob, File)] = existing
      .filter { case (_   , file) => !file.isDirectory }
      .filter { case (blob, file) => file.lastModified() > blob.getUpdateTime }
      .filter { case (blob, file) => blob.getCrc32cToHexString !=
          Strings.bytes2hex(Hashing.crc32c.hashBytes(Files.readFile(file)).asBytes.reverse)
      }
      .sortBy { case (blob, _   ) => blob.getName }
    log(s"Found ${toUpdate.length} blobs to update")


    if (toDelete.nonEmpty) {
      log(s"Deleting ${toDelete.length} blobs")
      for (blob <- toDelete) run(s"Deleting blob ${blob.getName}", blob.delete())
      log(s"Done deleting")
    }

    if (toCreateDirectories.nonEmpty) {
      log(s"Creating ${toCreateDirectories.length} new directory blobs")
      for (newDirectory <- toCreateDirectories) run(s"Creating new directory blob $newDirectory", createDirectoryBlob(newDirectory))
      log(s"Done creating directory blobs")
    }

    if (toUpload.nonEmpty) {
      log(s"Uploading ${toUpload.length} files")
      for ((name, file) <- toUpload) run(s"Uploading $file to $name", write(name, file))
      log(s"Done uploading")
    }

    if (toUpdate.nonEmpty) {
      log(s"Updating ${toUpdate.length} blobs")
      for ((blob, file) <- toUpdate) run(s"Updating ${blob.getName} from $file", write(blob.getName, file))
      log(s"Done updating")
    }
  }

  private def run(message: String, action: => Unit): Unit =
    if (dryRun)
      log(s"NOT $message")
    else {
      log(message)
      action
    }

  private def createDirectoryBlob(blobName: String): Unit =
    storage.create(BlobInfo.newBuilder(BlobId.of(bucketName, blobName)).build())

  private def write(blobName: String, file: File): Unit = {
    val contentType: String = URLConnection.getFileNameMap.getContentTypeFor(file.getName)
    val blobInfo = BlobInfo.newBuilder(BlobId.of(bucketName, blobName))
      .setContentType(contentType)
      .build()
    val content: Array[Byte] = Files.readFile(file)
    val writer: WriteChannel = storage.writer(blobInfo)
    writer.write(ByteBuffer.wrap(content, 0, content.length))
    writer.close()
  }

  private def listDirectory(directoryPath: String): Map[String, File] = {
    @tailrec
    def listDirectories(result: List[File], directoriesToList: List[File]): List[File] = directoriesToList match {
      case Nil => result
      case current :: tail =>
        val gsignore = new File(current, ".gsignore")
        val ignore: Set[String] = if (!gsignore.exists()) Set.empty else Files.read(gsignore).toSet + ".gsignore"
        val (directories: List[File], files: List[File]) = current.listFiles.toList
          .filterNot(file => ignore.contains(file.getName))
          .partition(_.isDirectory)
        listDirectories(result ++ directories ++ files, tail ++ directories)
    }

    listDirectories(List.empty, List(new File(directoryPath)))
      .map { file =>
        val name: String = Strings.drop(file.getAbsolutePath, directoryPath)
        (if (file.isDirectory) s"$name/" else name) -> file
      }
      .toMap
  }

  private def listBucketDirectory(bucketPrefix: String): Map[String, Blob] = {
    @tailrec
    def listDirectories(result: List[Blob], directoriesToList: List[String]): List[Blob] = directoriesToList match {
      case Nil => result
      case current :: tail =>
        val (directories: List[Blob], files: List[Blob]) = listBucketFiles(current).partition(isDirectory)
        // Note: list() includes the directory being listed it seems...
        val newDirectories: List[Blob] = directories.filterNot(directory => result.exists(_.getName == directory.getName))
        listDirectories(result ++ newDirectories ++ files, tail ++ newDirectories.map(_.getName))
    }

    listDirectories(List.empty, List(bucketPrefix))
      .map(blob => blob.getName -> blob)
      .toMap
  }

  private def listBucketFiles(bucketPrefix: String): List[Blob] = {
    @tailrec
    def listPages(result: List[Blob], page: Page[Blob]): List[Blob] =
      if (page == null) result
      else listPages(result ++ page.iterateAll().asScala.toList, page.getNextPage)

    listPages(
      result = List.empty,
      page = storage.list(
        bucketName,
        Storage.BlobListOption.currentDirectory(),
        Storage.BlobListOption.prefix(bucketPrefix)
      )
    )
  }

  private def isDirectory(blob: Blob): Boolean = blob.getName.endsWith("/")
}

object GoogleCloudStorageSynchronizer {

  def sync(
    serviceAccountKey: String,
    bucketName: String,
    bucketPrefix: String,
    directoryPath: String,
    dryRun: Boolean
  ): Unit = new GoogleCloudStorageSynchronizer(
    serviceAccountKey,
    bucketName,
    bucketPrefix,
    directoryPath,
    dryRun
  ).sync()
}
