package org.opentorah.files

import com.google.api.gax.paging.Page
import com.google.auth.oauth2.ServiceAccountCredentials
import com.google.cloud.WriteChannel
import com.google.cloud.storage.{Blob, BlobId, BlobInfo, Storage, StorageOptions}
import com.google.common.hash.Hashing
import org.opentorah.service.Credentials
import org.opentorah.util.{Files, Strings}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.URLConnection
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.IterableHasAsScala

// documentation: https://github.com/googleapis/java-storage
final class GoogleCloudStorageSynchronizer(
  serviceAccountKey: String,
  bucketName: String,
  bucketPrefix: String,
  directoryPath: String,
  dryRun: Boolean
):
  require(directoryPath.endsWith("/"))

  private val logger: Logger = LoggerFactory.getLogger(classOf[GoogleCloudStorageSynchronizer])

  private def log(message: String): Unit = logger.info(message)

  private val storage: Storage = StorageOptions.newBuilder()
    .setCredentials(Credentials.fromString(serviceAccountKey))
    .build()
    .getService

  private def isDirectory(blob: Blob): Boolean = blob.getName.endsWith("/")

  def sync(): Unit =
    log(s"Synchronizing $directoryPath to $bucketName/$bucketPrefix" + (if dryRun then " (dry run)" else ""))

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
      .filter((name, _) => !blobs.contains(name))
      .sortBy((name, _) => name)

    val toCreateDirectories: List[String] = newFiles
      .filter((_, file) => file.isDirectory)
      .map   ((name, _) => name)
    log(s"Found ${toCreateDirectories.length} directories to create")

    val toUpload: List[(String, File)] = newFiles
      .filter((_, file) => !file.isDirectory)
    log(s"Found ${toUpload.length} files to upload")

    val existing: List[(Blob, File)] = for
      (name, blob) <- blobs.toList
      file <- files.get(name)
    yield (blob, file)

    val flavourChanged: List[String] = existing
      .filter((blob, file) => isDirectory(blob) != file.isDirectory)
      .map   ((blob, file) =>
        def status(isDirectory: Boolean): String = if isDirectory then "is" else "is not "
        s"Blob $blob ${status(isDirectory(blob))} but file $file ${status(file.isDirectory)}"
      )

    if flavourChanged.nonEmpty then
      throw IllegalArgumentException("isDirectory changes: " + flavourChanged.mkString("\n"))

    val toUpdate: List[(Blob, File)] = existing
      .filter((_   , file) => !file.isDirectory)
      .filter((blob, file) => file.lastModified() > blob.getUpdateTimeOffsetDateTime.toInstant.toEpochMilli)
      .filter((blob, file) => blob.getCrc32cToHexString !=
        Strings.bytes2hex(Hashing.crc32c.hashBytes(Files.readBytes(file)).asBytes.toIndexedSeq.reverse)
      )
      .sortBy((blob, _   ) => blob.getName)
    log(s"Found ${toUpdate.length} blobs to update")


    if toDelete.nonEmpty then
      log(s"Deleting ${toDelete.length} blobs")
      for blob <- toDelete do run(s"Deleting blob ${blob.getName}", blob.delete())
      log(s"Done deleting")

    if toCreateDirectories.nonEmpty then
      log(s"Creating ${toCreateDirectories.length} new directory blobs")
      for newDirectory <- toCreateDirectories do run(s"Creating new directory blob $newDirectory", createDirectoryBlob(newDirectory))
      log(s"Done creating directory blobs")

    if toUpload.nonEmpty then
      log(s"Uploading ${toUpload.length} files")
      for (name, file) <- toUpload do run(s"Uploading $file to $name", write(name, file))
      log(s"Done uploading")

    if toUpdate.nonEmpty then
      log(s"Updating ${toUpdate.length} blobs")
      for (blob, file) <- toUpdate do run(s"Updating ${blob.getName} from $file", write(blob.getName, file))
      log(s"Done updating")

  private def run(message: String, action: => Unit): Unit =
    if dryRun then
      log(s"NOT $message")
    else
      log(message)
      action

  private def createDirectoryBlob(blobName: String): Unit =
    storage.create(BlobInfo.newBuilder(BlobId.of(bucketName, blobName)).build())

  private def write(blobName: String, file: File): Unit =
    val contentType: String = URLConnection.getFileNameMap.getContentTypeFor(file.getName)
    val blobInfo = BlobInfo.newBuilder(BlobId.of(bucketName, blobName))
      .setContentType(contentType)
      .build()
    val content: Array[Byte] = Files.readBytes(file)
    val writer: WriteChannel = storage.writer(blobInfo)
    writer.write(ByteBuffer.wrap(content, 0, content.length))
    writer.close()

  private def listDirectory(directoryPath: String): Map[String, File] =
    @tailrec
    def listDirectories(result: List[File], directoriesToList: List[File]): List[File] =
      if directoriesToList.isEmpty then result else
        val current: File = directoriesToList.head
        val gsIgnore: File = File(current, GoogleCloudStorageSynchronizer.ignoreFileName)
        val ignore: Set[String] =
          if !gsIgnore.exists
          then Set.empty
          else Files.read(gsIgnore).toSet + GoogleCloudStorageSynchronizer.ignoreFileName
        val (directories: List[File], files: List[File]) = current.listFiles.toList
          .filterNot(file => ignore.contains(file.getName))
          .partition(_.isDirectory)
        listDirectories(result ++ directories ++ files, directoriesToList.tail ++ directories)

    listDirectories(List.empty, List(File(directoryPath)))
      .map(file =>
        val name: String = Strings.drop(file.getAbsolutePath, directoryPath)
        (if file.isDirectory then s"$name/" else name) -> file
      )
      .toMap

  private def listBucketDirectory(bucketPrefix: String): Map[String, Blob] =
    @tailrec
    def listDirectories(result: List[Blob], directoriesToList: List[String]): List[Blob] =
      if directoriesToList.isEmpty then result else
        val (directories: List[Blob], files: List[Blob]) = listBucketFiles(directoriesToList.head).partition(isDirectory)
        // Note: list() includes the directory being listed it seems...
        val newDirectories: List[Blob] = directories.filterNot(directory => result.exists(_.getName == directory.getName))
        listDirectories(result ++ newDirectories ++ files, directoriesToList.tail ++ newDirectories.map(_.getName))

    listDirectories(List.empty, List(bucketPrefix))
      .map(blob => blob.getName -> blob)
      .toMap

  private def listBucketFiles(bucketPrefix: String): List[Blob] =
    @tailrec
    def listPages(result: List[Blob], page: Page[Blob]): List[Blob] =
      if page == null then result
      else listPages(result ++ page.iterateAll().asScala.toList, page.getNextPage)

    listPages(
      result = List.empty,
      page = storage.list(
        bucketName,
        Storage.BlobListOption.currentDirectory(),
        Storage.BlobListOption.prefix(bucketPrefix)
      )
    )

object GoogleCloudStorageSynchronizer:
  val ignoreFileName: String = ".gsignore"
