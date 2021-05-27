package uk.gov.nationalarchives

import org.apache.commons.io.FilenameUtils
import uk.gov.nationalarchives.DroidClient.{FFIDMetadataInput, FFIDMetadataInputMatches}
import uk.gov.nationalarchives.droid.core.BinarySignatureIdentifier
import uk.gov.nationalarchives.droid.core.interfaces.resource.{FileSystemIdentificationRequest, RequestMetaData}
import uk.gov.nationalarchives.droid.core.interfaces.{IdentificationRequest, IdentificationResult, IdentificationResultCollection, RequestIdentifier}

import java.nio.file.{Files, Path}
import java.util.Date
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try

class DroidClient(signatureFileLocation: String) {
  val signatureFileName = "DROID_SignatureFile_V96.xml"
  val containerFileName = "container-signature-20200121.xml"
  val droidVersion = "6.5"


  private def metadata(path: Path): RequestMetaData = {
    val size = Files.size(path)
    val lastModified = Option(Files.getLastModifiedTime(path))
      .map(lm => new Date(lm.toMillis).getTime)
      .getOrElse(new Date(0).getTime)
    val name = path.getFileName.toString

    new RequestMetaData(size, lastModified, name)
  }

  def getBinarySignatureIdentifier: BinarySignatureIdentifier = {
    val binarySignatureIdentifier = new BinarySignatureIdentifier()

    binarySignatureIdentifier.setSignatureFile(signatureFileLocation)
    binarySignatureIdentifier.init()
    binarySignatureIdentifier
  }

  def fileIdRequest(path: Path): FileSystemIdentificationRequest = {
    val identifier = new RequestIdentifier(path.toUri)
    identifier.setParentResourceId(null)
    identifier.setResourceId(null)

    val request = new FileSystemIdentificationRequest(metadata(path), identifier)
    request.open(path)
    request
  }

  def results(path: Path): FFIDMetadataInput = {
    val binarySignatureIdentifier = getBinarySignatureIdentifier
    val request = fileIdRequest(path)
    val binarySignatureResults: IdentificationResultCollection = binarySignatureIdentifier.matchBinarySignatures(request)
    val results = handleExtensions(binarySignatureIdentifier, request, binarySignatureResults)
    val extension = getExtension(path.getFileName.toString)
    val matches = results.getResults.asScala.toList match {
      case Nil =>
        List(FFIDMetadataInputMatches(extension, "", Option.empty))
      case identificationResults: List[IdentificationResult] =>
        identificationResults.map(res => FFIDMetadataInputMatches(extension, res.getMethod.getMethod, Option(res.getPuid)))
    }
    FFIDMetadataInput("Droid", droidVersion, containerFileName, signatureFileName, "pronom", matches)
  }

  def getExtension(filename: String): Option[String] = {
    val queryPos = filename.indexOf('?')
    val bareFilename = if(queryPos > -1) { filename.substring(0, queryPos) } else filename
    val nameOnly = FilenameUtils.getName(bareFilename)
    val dotPos = nameOnly.lastIndexOf('.')
    if(dotPos > 0)  {Option(nameOnly.substring(dotPos + 1)) } else None
  }

  def handleExtensions(binarySignatureIdentifier: BinarySignatureIdentifier, request: IdentificationRequest[_], results: IdentificationResultCollection): IdentificationResultCollection = {
    Try {
      val resultList = results.getResults
      if (resultList.isEmpty) {
        val a: IdentificationResultCollection = binarySignatureIdentifier.matchExtensions(request, false)
        Option(a).getOrElse(results)
      }
      else {
        binarySignatureIdentifier.checkForExtensionsMismatches(results, request.getExtension)
        results
      }
    }.getOrElse(results)
  }
}
object DroidClient {
  def apply(signatureFileLocation: String) = new DroidClient(signatureFileLocation)
  case class FFIDMetadataInputMatches(extension : Option[String], identificationBasis : String, puid : Option[String])
  case class FFIDMetadataInput(software : String, softwareVersion : String, binarySignatureFileVersion : String, containerSignatureFileVersion : String, method : String, matches : scala.List[FFIDMetadataInputMatches])
}