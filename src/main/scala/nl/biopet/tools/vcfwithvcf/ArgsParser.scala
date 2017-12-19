/*
 * Copyright (c) 2014 Sequencing Analysis Support Core - Leiden University Medical Center
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

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[File]('I', "inputFile") required () maxOccurs 1 valueName "<file>" action {
    (x, c) =>
      c.copy(inputFile = x)
  } text "Input VCF file"
  opt[File]('s', "secondaryVcf") required () maxOccurs 1 valueName "<file>" action {
    (x, c) =>
      c.copy(secondaryVcf = x)
  } text "Second Input VCF file"
  opt[File]('o', "outputFile") required () maxOccurs 1 valueName "<file>" action {
    (x, c) =>
      c.copy(outputFile = x)
  } text "Output VCF file."
  opt[File]('R', "reference") required () maxOccurs 1 valueName "<file>" action {
    (x, c) =>
      c.copy(referenceFasta = x)
  } text "The refence that was used to call the VCF"
  opt[String]('f', "field") unbounded () valueName "<field> or <input_field:output_field> or <input_field:output_field:method>" action {
    (x, c) =>
      val values = x.split(":")
      if (values.size > 2)
        c.copy(
          fields = Fields(values(0),
                          values(1),
                          FieldMethod.withName(values(2))) :: c.fields)
      else if (values.size > 1)
        c.copy(fields = Fields(values(0), values(1)) :: c.fields)
      else c.copy(fields = Fields(x, x) :: c.fields)
  } text """| If only <field> is given, the field's identifier in the output VCF will be identical to <field>.
            | By default we will return all values found for a given field.
            | For INFO fields with type R or A we will take the respective alleles present in the input file.
            | If a <method> is supplied, a method will be applied over the contents of the field.
            | In this case, all values will be considered.
            | The following methods are available:
            |   - max   : takes maximum of found value, only works for numeric (integer/float) fields
            |   - min   : takes minimum of found value, only works for numeric (integer/float) fields
            |   - unique: takes only unique values """.stripMargin
  opt[Boolean]("match") valueName "<Boolean>" maxOccurs 1 action { (x, c) =>
    c.copy(matchAllele = x)
  } text "Match alternative alleles; default true"
}
