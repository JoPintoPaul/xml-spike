/**
  * Created by joanna.pinto on 24/04/2018.
  */
import java.io.{InputStream, InputStreamReader}

import javax.xml.stream.{XMLInputFactory, XMLStreamException, XMLStreamReader}

import scala.util.{Failure, Success, Try}

object RelyingOnExceptionXMLSafetyChecker {

  def checkIfSafe(source: InputStream): Boolean = {
    val targetReader: InputStreamReader = new InputStreamReader(source)

    val streamReader = getSecureXMLInputFactory().createXMLStreamReader(targetReader)
    val result       = Try(readStreamUntilEntityExpansion(streamReader))

    targetReader.close()

    result match {
      case Success(_) => true
      case Failure(e: XMLStreamException) if e.getMessage contains "Message: The entity" =>
        false
      case Failure(e) =>
        true
    }

  }

  private def getSecureXMLInputFactory(): XMLInputFactory = {
    val xmlInputFactory = XMLInputFactory.newFactory()
    xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false)

    xmlInputFactory
  }

  private def readStreamUntilEntityExpansion(streamReader: XMLStreamReader) =
    while (streamReader.hasNext) {
      streamReader.next()
    }
}
