package nl.biopet.tools.vcfwithvcf

import java.io.File

case class Args(inputFile: File = null,
                outputFile: File = null,
                referenceFasta: File = null,
                secondaryVcf: File = null,
                fields: List[Fields] = Nil,
                matchAllele: Boolean = true)
