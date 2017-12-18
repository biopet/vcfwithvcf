package nl.biopet.tools.vcfwithvcf

import java.io.File

import nl.biopet.utils.tool.AbstractOptParser

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
