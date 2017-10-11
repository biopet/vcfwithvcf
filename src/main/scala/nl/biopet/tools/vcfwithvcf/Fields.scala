package nl.biopet.tools.vcfwithvcf

case class Fields(inputField: String,
                  outputField: String,
                  fieldMethod: FieldMethod.Value = FieldMethod.none)
