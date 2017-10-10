package nl.biopet.tools.vcfwithvcf

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

object VcfWithVcfTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      ToolTemplate.main(Array())
    }
  }
}
