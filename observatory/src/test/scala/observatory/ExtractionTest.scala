package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  
  import Extraction._

  @Test def `'locateTemperature test'`:Unit = {
  locateTemperatures(1975,"/stations.csv","/1975.csv")
  }
}
