package nl.biopet.tools.vcfwithvcf

import java.util

import htsjdk.variant.variantcontext.writer.{
  AsyncVariantContextWriter,
  VariantContextWriterBuilder
}
import htsjdk.variant.variantcontext.{VariantContext, VariantContextBuilder}
import htsjdk.variant.vcf._
import nl.biopet.utils.ngs.fasta
import nl.biopet.utils.ngs.bam.SamDictCheck
import nl.biopet.utils.tool.ToolCommand
import nl.biopet.utils.conversions.scalaListToJavaObjectArrayList

import scala.collection.JavaConversions._

object VcfWithVcf extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)
  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val reader = new VCFFileReader(cmdArgs.inputFile, false)
    val secondaryReader = new VCFFileReader(cmdArgs.secondaryVcf)

    val referenceDict = fasta.getCachedDict(cmdArgs.referenceFasta)

    val header = reader.getFileHeader
    val vcfDict = header.getSequenceDictionary match {
      case r if r != null =>
        r.assertSameDictionary(referenceDict, true)
        r
      case _ => referenceDict
    }
    val secondHeader = secondaryReader.getFileHeader

    secondHeader.getSequenceDictionary match {
      case r if r != null => r.assertSameDictionary(referenceDict, true)
      case _ =>
    }

    val writer = new AsyncVariantContextWriter(
      new VariantContextWriterBuilder()
        .setOutputFile(cmdArgs.outputFile)
        .setReferenceDictionary(vcfDict)
        .build)

    for (x <- cmdArgs.fields) {
      if (header.hasInfoLine(x.outputField))
        throw new IllegalArgumentException(
          "Field '" + x.outputField + "' already exists in input vcf")
      if (!secondHeader.hasInfoLine(x.inputField))
        throw new IllegalArgumentException(
          "Field '" + x.inputField + "' does not exist in secondary vcf")

      val oldHeaderLine = secondHeader.getInfoHeaderLine(x.inputField)

      val newHeaderLine = new VCFInfoHeaderLine(x.outputField,
                                                VCFHeaderLineCount.UNBOUNDED,
                                                oldHeaderLine.getType,
                                                oldHeaderLine.getDescription)
      header.addMetaDataLine(newHeaderLine)
    }
    writer.writeHeader(header)

    logger.info("Start reading records")

    var counter = 0
    for (record <- reader) {
      require(vcfDict.getSequence(record.getContig) != null,
              s"Contig ${record.getContig} does not exist on reference")
      val secondaryRecords =
        getSecondaryRecords(secondaryReader, record, cmdArgs.matchAllele)

      val fieldMap =
        createFieldMap(cmdArgs.fields, record, secondaryRecords, secondHeader)

      writer.add(createRecord(fieldMap, record, cmdArgs.fields, header))

      counter += 1
      if (counter % 100000 == 0) {
        logger.info(s"""Processed $counter records""")
      }
    }
    logger.info(s"""Processed $counter records""")

    logger.debug("Closing readers")
    writer.close()
    reader.close()
    secondaryReader.close()
    logger.info("Done")
  }

  /**
    * Create Map of field -> List of attributes in secondary records
    * @param fields List of Field
    * @param record Original record
    * @param secondaryRecords List of VariantContext with secondary records
    * @param header: header of secondary reader
    * @return Map of fields and their values in secondary records
    */
  def createFieldMap(fields: List[Fields],
                     record: VariantContext,
                     secondaryRecords: List[VariantContext],
                     header: VCFHeader): Map[String, List[Any]] = {
    val fieldMap =
      (for (f <- fields
            if secondaryRecords.exists(_.hasAttribute(f.inputField))) yield {
        f.outputField -> (for (secondRecord <- secondaryRecords
                               if secondRecord.hasAttribute(f.inputField))
          yield {
            getSecondaryField(record, secondRecord, f.inputField, header) match {
              case l: List[_] => l
              case y: util.ArrayList[_] => y.toList
              case x => List(x)
            }
          }).fold(Nil)(_ ::: _)
      }).toMap
    fieldMap
  }

  /**
    * Get secondary records matching the query record
    * @param secondaryReader reader for secondary records
    * @param record query record
    * @param matchAllele allele has to match query allele?
    * @return List of VariantContext
    */
  def getSecondaryRecords(secondaryReader: VCFFileReader,
                          record: VariantContext,
                          matchAllele: Boolean): List[VariantContext] = {
    if (matchAllele) {
      secondaryReader
        .query(record.getContig, record.getStart, record.getEnd)
        .filter(x => record.getAlternateAlleles.exists(x.hasAlternateAllele))
        .toList
    } else {
      secondaryReader
        .query(record.getContig, record.getStart, record.getEnd)
        .toIterable
        .toList
    }
  }

  def createRecord(fieldMap: Map[String, List[Any]],
                   record: VariantContext,
                   fields: List[Fields],
                   header: VCFHeader): VariantContext = {
    fieldMap
      .foldLeft(new VariantContextBuilder(record))((builder, attribute) => {
        val field = fields.filter(_.outputField == attribute._1).head
        builder.attribute(
          attribute._1,
          field.fieldMethod match {
            case FieldMethod.max =>
              header.getInfoHeaderLine(attribute._1).getType match {
                case VCFHeaderLineType.Integer =>
                  attribute._2.map(_.toString.toInt).max
                case VCFHeaderLineType.Float =>
                  attribute._2
                    .map(_.toString.toFloat)
                    .max
                    .toString
                    .replace("E-", "e-")
                case _ =>
                  throw new IllegalArgumentException(
                    "Type of field " + field.inputField + " is not numeric")
              }
            case FieldMethod.min =>
              header.getInfoHeaderLine(attribute._1).getType match {
                case VCFHeaderLineType.Integer =>
                  attribute._2.map(_.toString.toInt).min
                case VCFHeaderLineType.Float =>
                  attribute._2
                    .map(_.toString.toFloat)
                    .min
                    .toString
                    .replace("E-", "e-")
                case _ =>
                  throw new IllegalArgumentException(
                    "Type of field " + field.inputField + " is not numeric")
              }
            case FieldMethod.unique =>
              scalaListToJavaObjectArrayList(attribute._2.distinct)
            case _ =>
              scalaListToJavaObjectArrayList(attribute._2)
          }
        )
      })
      .make()
  }

  /**
    * Get the proper representation of a field from a secondary record given an original record
    * @param record original record
    * @param secondaryRecord secondary record
    * @param field field
    * @param header header of secondary record
    * @return
    */
  def getSecondaryField(record: VariantContext,
                        secondaryRecord: VariantContext,
                        field: String,
                        header: VCFHeader): Any = {
    header.getInfoHeaderLine(field).getCountType match {
      case VCFHeaderLineCount.A => numberA(record, secondaryRecord, field)
      case VCFHeaderLineCount.R => numberR(record, secondaryRecord, field)
      case _ => secondaryRecord.getAttribute(field)
    }
  }

  /**
    * Get the correct values from a field that has number=A
    * @param referenceRecord the reference record
    * @param annotateRecord the to-be-annotated record
    * @param field the field to annotate
    * @return
    */
  def numberA(referenceRecord: VariantContext,
              annotateRecord: VariantContext,
              field: String): List[Any] = {
    val refValues = annotateRecord.getAttributeAsList(field).toArray
    referenceRecord.getAlternateAlleles
      .filter(referenceRecord.hasAlternateAllele)
      .map(x => referenceRecord.getAlternateAlleles.indexOf(x))
      .flatMap(x => refValues.lift(x))
      .toList
  }

  /**
    * Get the correct values from a field that has number=R
    * @param referenceRecord the reference record
    * @param annotateRecord the to-be-annotated record
    * @param field the field to annotate
    * @return
    */
  def numberR(referenceRecord: VariantContext,
              annotateRecord: VariantContext,
              field: String): List[Any] = {
    val refValues = annotateRecord.getAttributeAsList(field).toArray
    referenceRecord.getAlleles
      .filter(referenceRecord.hasAllele)
      .map(x => referenceRecord.getAlleles.indexOf(x))
      .flatMap(x => refValues.lift(x))
      .toList
  }
}
