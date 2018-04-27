import java.io.{ByteArrayInputStream, InputStream, InputStreamReader}
import javax.xml.stream.events.{DTD, EntityDeclaration, XMLEvent}
import javax.xml.stream.{XMLEventReader, XMLInputFactory, XMLResolver, XMLStreamReader}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object ExplicitAnalysisXMLSafetyChecker {

  def checkIfDtdSafe(source: InputStream): Boolean =
    extractDtdDeclarations(source) match {
      case Success((dtdDeclarations, systemIdsWeTriedToResolve)) =>
        for {
          declaration <- dtdDeclarations
        } {
          declaration.getNotations
        }

        val entitySystemIds = for {
          declaration <- dtdDeclarations
          entity <- Option(declaration.getEntities).map(_.asScala).getOrElse(Nil).map(_.asInstanceOf[EntityDeclaration])
          systemId <- Option(entity.getSystemId)
        } yield systemId
        entitySystemIds.isEmpty && systemIdsWeTriedToResolve.isEmpty

      case Failure(e) =>
        println(e)
        true //Not an XML
    }

  private def extractDtdDeclarations(source: InputStream) = {
    val systemIdsTriedToResolve = new scala.collection.concurrent.TrieMap[String, Unit]()

    def handleEntityResolver(systemId: String) =
      systemIdsTriedToResolve.put(systemId, ())

    val targetReader: InputStreamReader = new InputStreamReader(source)
    val streamReader: XMLEventReader = buildXmlInputFactory(handleEntityResolver).createXMLEventReader(targetReader)

    val eventStream = streamReader.asScala.toStream.map(_.asInstanceOf[XMLEvent])

    Try {

      val validDtdDeclarations = eventStream
        .takeWhile(!_.isStartElement)
        .collect { case dtd: DTD => dtd }
        .toSeq

      val immutableResolvedEntities = systemIdsTriedToResolve.keys.toSeq

      (validDtdDeclarations, immutableResolvedEntities)
    }

  }

  def checkIfBillionLaughsSafe(source: InputStream): Boolean = {
    def readStream(streamReader: XMLStreamReader): Unit =
      while (streamReader.hasNext) {
        streamReader.next()
      }

    val xmlInputFactory = XMLInputFactory.newFactory()
    val targetReader: InputStreamReader = new InputStreamReader(source)
    val streamReader = xmlInputFactory.createXMLStreamReader(targetReader)
    val result = Try(readStream(streamReader))
    targetReader.close()

    result match {
      case Success(_) => true
      case Failure(t: Throwable) => !t.getMessage.contains("JAXP00010001")
    }
  }

  private def buildXmlInputFactory(resolvedEntityHandler: String => Unit): XMLInputFactory = {
    val xmlInputFactory = XMLInputFactory.newFactory()
    xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, true)
    xmlInputFactory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, true)
    xmlInputFactory.setProperty(
      XMLInputFactory.RESOLVER,
      new XMLResolver {
        override def resolveEntity(publicId: String, systemId: String, baseUri: String, namespace: String): AnyRef = {
          resolvedEntityHandler(systemId)
          new ByteArrayInputStream(new Array[Byte](0))
        }
      }
    )

    xmlInputFactory
  }
}
