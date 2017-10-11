package nl.biopet.tools.vcfwithvcf

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class VcfWithVcfTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      VcfWithVcf.main(Array())
    }
  }
}
