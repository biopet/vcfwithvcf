/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.vcfwithvcf

import java.io.File
import java.util

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.utils.ngs.vcf.BiopetVariantContext
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

import scala.collection.JavaConversions._
import scala.util.Random

class VcfWithVcfTest extends ToolTest[Args] {
  def toolCommand: VcfWithVcf.type = VcfWithVcf
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(Array())
    }
  }

  val samplePath: String = resourcePath("/sample.vcf")
  val wrongContigPath: String = resourcePath("/wrong_contig.vcf")
  val annotationPath: String = resourcePath("/annotation.vcf")
  val veppedPath: String = resourcePath("/VEP_oneline.vcf.gz")
  val unveppedPath: String = resourcePath("/unvep_online.vcf.gz")
  val referenceFasta: String = resourcePath("/fake_chrQ.fa")
  val monoPath: String = resourcePath("/chrQ_monoallelic.vcf.gz")
  val multiPath: String = resourcePath("/chrQ_multiallelic.vcf.gz")
  val rand = new Random()

  def readRecords(file: File): List[VariantContext] = {
    val reader = new VCFFileReader(file, false)
    val records = reader.toList
    reader.close()
    records
  }

  def testSimpleValues(outputFile: File): Unit = {
    val arguments = Array(
      "-I",
      samplePath,
      "-s",
      annotationPath,
      "-o",
      outputFile.getAbsolutePath,
      "-f",
      "INT",
      "-f",
      "DOUBLE",
      "-f",
      "STRING",
      "-f",
      "MULTI_INT:INT_MIN:min",
      "-f",
      "MULTI_INT:INT_MAX:max",
      "-f",
      "MULTI_DOUBLE:DOUBLE_MIN:min",
      "-f",
      "MULTI_DOUBLE:DOUBLE_MAX:max",
      "-f",
      "MULTI_STRING:STRING_unique:unique",
      "-R",
      referenceFasta
    )
    VcfWithVcf.main(arguments)
    val record = readRecords(outputFile).head
    record.hasAttribute("INT") shouldBe true
    record.getAttributeAsInt("INT", -1) shouldBe 3
    record.hasAttribute("STRING") shouldBe true
    record.getAttribute("STRING") shouldBe "bla"
    record.hasAttribute("INT_MIN") shouldBe true
    record.getAttributeAsInt("INT_MIN", -1) shouldBe 3
    record.hasAttribute("INT_MAX") shouldBe true
    record.getAttributeAsInt("INT_MAX", -1) shouldBe 6
    record.hasAttribute("STRING_unique") shouldBe true
    record.getAttribute("STRING_unique") shouldBe "bla"

    record.hasAttribute("DOUBLE") shouldBe true
    record.hasAttribute("DOUBLE_MIN") shouldBe true
    record.hasAttribute("DOUBLE_MAX") shouldBe true
    if (!outputFile.getName.endsWith(".bcf")) { // BCF file seems to have rounding errors
      record.getAttributeAsDouble("DOUBLE", -1.0) shouldBe 0.3
      record.getAttributeAsDouble("DOUBLE_MIN", -1.0) shouldBe 3.01e-6
      record.getAttribute("DOUBLE_MIN") shouldBe "3.01e-6"
      record.getAttributeAsDouble("DOUBLE_MAX", -1.0) shouldBe 0.3
    }
  }

  @Test
  def testOutputTypeVcf(): Unit = {
    val tmpFile = File.createTempFile("VcfWithVcf.", ".vcf")
    tmpFile.deleteOnExit()
    testSimpleValues(tmpFile)
  }

  @Test
  def testOutputTypeVcfGz(): Unit = {
    val tmpFile = File.createTempFile("VcfWithVcf.", ".vcf.gz")
    tmpFile.deleteOnExit()
    testSimpleValues(tmpFile)
  }

  @Test
  def testOutputFieldException(): Unit = {
    val tmpFile = File.createTempFile("VCFWithVCf.", ".vcf")
    tmpFile.deleteOnExit()
    val args = Array("-I",
                     samplePath,
                     "-s",
                     annotationPath,
                     "-o",
                     tmpFile.getAbsolutePath,
                     "-f",
                     "INT:DP",
                     "-R",
                     referenceFasta)
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(args)
    }.getMessage shouldBe "Field 'DP' already exists in input vcf"
  }

  @Test
  def testWrongContig(): Unit = {
    val tmpFile = File.createTempFile("VCFWithVCf.", ".vcf")
    tmpFile.deleteOnExit()
    val args = Array("-I",
                     wrongContigPath,
                     "-s",
                     annotationPath,
                     "-o",
                     tmpFile.getAbsolutePath,
                     "-f",
                     "INT",
                     "-R",
                     referenceFasta)
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(args)
    }.getMessage shouldBe "requirement failed: Contig chrX does not exist on reference"
  }

  @Test
  def testInputFieldException(): Unit = {
    val tmpFile = File.createTempFile("VCFWithVCf.", ".vcf")
    tmpFile.deleteOnExit()
    val args = Array("-I",
                     samplePath,
                     "-s",
                     annotationPath,
                     "-o",
                     tmpFile.getAbsolutePath,
                     "-f",
                     "CSQ:NEW_CSQ",
                     "-R",
                     referenceFasta)
    an[IllegalArgumentException] should be thrownBy VcfWithVcf.main(args)
    val thrown = the[IllegalArgumentException] thrownBy VcfWithVcf.main(args)
    thrown.getMessage should equal(
      "Field 'CSQ' does not exist in secondary vcf")
  }

  @Test
  def testMinMethodException(): Unit = {
    val tmpFile = File.createTempFile("VcfWithVcf_", ".vcf")
    tmpFile.deleteOnExit()
    val args = Array("-I",
                     samplePath,
                     "-s",
                     annotationPath,
                     "-o",
                     tmpFile.getAbsolutePath,
                     "-f",
                     "MULTI_STRING:STRING:min",
                     "-R",
                     referenceFasta)
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(args)
    }.getMessage shouldBe "Type of field Some(MULTI_STRING) is not numeric"
  }

  @Test
  def testMaxMethodException(): Unit = {
    val tmpFile = File.createTempFile("VcfWithVcf_", ".vcf")
    tmpFile.deleteOnExit()
    val args = Array("-I",
                     samplePath,
                     "-s",
                     annotationPath,
                     "-o",
                     tmpFile.getAbsolutePath,
                     "-f",
                     "MULTI_STRING:STRING:max",
                     "-R",
                     referenceFasta)
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(args)
    }.getMessage shouldBe "Type of field Some(MULTI_STRING) is not numeric"
  }

  @Test
  def testFieldMap(): Unit = {
    val unvepReader = new VCFFileReader(new File(unveppedPath))
    val header = unvepReader.getFileHeader
    val unvepRecord = unvepReader.iterator().next()

    var fields = List(Fields("FG", "FG"))
    fields :::= List(Fields("FD", "FD"))
    fields :::= List(Fields("GM", "GM"))
    fields :::= List(Fields("GL", "GL"))
    fields :::= List(Fields("CP", "CP"))
    fields :::= List(Fields("CG", "CG"))
    fields :::= List(Fields("CN", "CN"))
    fields :::= List(Fields("DSP", "DSP"))
    fields :::= List(Fields("AC", "AC"))
    fields :::= List(Fields("AF", "AF"))
    fields :::= List(Fields("AN", "AN"))
    fields :::= List(Fields("BaseQRankSum", "BaseQRankSum"))
    fields :::= List(Fields("DP", "DP"))
    fields :::= List(Fields("FS", "FS"))
    fields :::= List(Fields("MLEAC", "MLEAC"))
    fields :::= List(Fields("MLEAF", "MLEAF"))
    fields :::= List(Fields("MQ", "MQ"))
    fields :::= List(Fields("MQ0", "MQ0"))
    fields :::= List(Fields("MQRankSum", "MQRankSum"))
    fields :::= List(Fields("QD", "QD"))
    fields :::= List(Fields("RPA", "RPA"))
    fields :::= List(Fields("RU", "RU"))
    fields :::= List(Fields("ReadPosRankSum", "ReadPosRankSum"))
    fields :::= List(Fields("VQSLOD", "VQSLOD"))
    fields :::= List(Fields("culprit", "culprit"))

    val fieldMap =
      VcfWithVcf.createFieldMap(fields, unvepRecord, List(unvepRecord), header)

    fieldMap("FG") shouldBe List("intron")
    fieldMap("FD") shouldBe List("unknown")
    fieldMap("GM") shouldBe List("NM_152486.2")
    fieldMap("GL") shouldBe List("SAMD11")
    fieldMap("CP") shouldBe List("0.000")
    fieldMap("CG") shouldBe List("-1.630")
    fieldMap("CN") shouldBe List("2294", "3274", "30362", "112930")
    fieldMap("DSP") shouldBe List("107")
    fieldMap("AC") shouldBe List("2")
    fieldMap("AF") shouldBe List("0.333")
    fieldMap("AN") shouldBe List("6")
    fieldMap("DP") shouldBe List("124")
    fieldMap("FS") shouldBe List("1.322")
    fieldMap("MLEAC") shouldBe List("2")
    fieldMap("MLEAF") shouldBe List("0.333")
    fieldMap("MQ") shouldBe List("60.0")
    fieldMap("MQ0") shouldBe List("0")
    fieldMap("MQRankSum") shouldBe List("-0.197")
    fieldMap("QD") shouldBe List("19.03")
    fieldMap("RPA") shouldBe List("1", "2")
    fieldMap("RU") shouldBe List("A")
    fieldMap("ReadPosRankSum") shouldBe List("-0.424")
    fieldMap("VQSLOD") shouldBe List("0.079")
    fieldMap("culprit") shouldBe List("FS")

  }

  @Test
  def testGetSecondaryRecords(): Unit = {
    val unvepRecord =
      new VCFFileReader(new File(unveppedPath)).iterator().next()
    val vepReader = new VCFFileReader(new File(veppedPath))
    val vepRecord = vepReader.iterator().next()

    val secRec = VcfWithVcf.getSecondaryRecords(vepReader,
                                                unvepRecord,
                                                matchAllele = false)

    secRec.foreach(x => x.identicalVariantContext(vepRecord) shouldBe true)
  }

  @Test
  def testCreateRecord(): Unit = {
    val unvepRecord =
      new VCFFileReader(new File(unveppedPath)).iterator().next()
    val vepReader = new VCFFileReader(new File(veppedPath))
    val header = vepReader.getFileHeader
    val vepRecord = vepReader.iterator().next()

    val secRec = VcfWithVcf.getSecondaryRecords(vepReader,
                                                unvepRecord,
                                                matchAllele = false)

    val fieldMap = VcfWithVcf.createFieldMap(List(Fields("CSQ", "CSQ")),
                                             vepRecord,
                                             secRec,
                                             header)
    val createdRecord = VcfWithVcf.createRecord(fieldMap,
                                                unvepRecord,
                                                List(Fields("CSQ", "CSQ")),
                                                header)
    createdRecord.identicalVariantContext(vepRecord) shouldBe true
  }

  @Test
  def testNumberA(): Unit = {
    val multiRecord = new VCFFileReader(new File(multiPath)).iterator().next()
    val monoRecord = new VCFFileReader(new File(monoPath)).iterator().next()

    val annot = VcfWithVcf.numberA(multiRecord, monoRecord, "AF")
    annot shouldBe List("0.333")

  }

  @Test
  def testNumberR(): Unit = {
    val multiRecord = new VCFFileReader(new File(multiPath)).iterator().next()
    val monoRecord = new VCFFileReader(new File(monoPath)).iterator().next()
    val annot = VcfWithVcf.numberR(multiRecord, monoRecord, "ALL_ALLELE")

    annot shouldBe List("C", "A")
  }

  @Test
  def testNumberAOutput(): Unit = {
    val tmpFile = File.createTempFile("numberA", ".vcf.gz")
    tmpFile.deleteOnExit()
    val arguments = Array("-I",
                          monoPath,
                          "-s",
                          multiPath,
                          "-o",
                          tmpFile.getAbsolutePath,
                          "-f",
                          "AF:MULTI_AF",
                          "-R",
                          referenceFasta)
    VcfWithVcf.main(arguments)
    val annotatedRecord = new VCFFileReader(tmpFile).iterator().next()
    annotatedRecord.getAttribute("MULTI_AF").toString shouldBe "0.333"

  }

  @Test
  def testNumberROutput(): Unit = {
    val tmpFile = File.createTempFile("numberR", ".vcf.gz")
    tmpFile.deleteOnExit()
    val arguments = Array("-I",
                          monoPath,
                          "-s",
                          multiPath,
                          "-o",
                          tmpFile.getAbsolutePath,
                          "-f",
                          "ALL_ALLELE:MULTI_ALL_ALLELE",
                          "-R",
                          referenceFasta)
    VcfWithVcf.main(arguments)
    val annotatedRecord = new VCFFileReader(tmpFile).iterator().next()
    annotatedRecord.getAttribute("MULTI_ALL_ALLELE") match {
      case l: List[_]           => l shouldBe List("C", "A")
      case u: util.ArrayList[_] => u.toList shouldBe List("C", "A")
      case _                    => throw new IllegalStateException("Not a list")
    }
  }

}
