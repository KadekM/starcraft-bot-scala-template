import bwapi.{Unit => ScUnit, _}
import bwta.BWTA

object TestBot {
  def main(args: Array[String]) =
    new TestBot().run()
}

class TestBot extends DefaultBWListener {
    val mirror = new Mirror()
    var game: Game = _
    var self: Player = _

    def run(): Unit = {
        mirror.getModule().setEventListener(this)
        mirror.startGame()
    }

    override def onUnitCreate(unit: ScUnit): Unit = {
        System.out.println("New unit " + unit.getType)
    }

    override def onStart(): Unit = {
        game = mirror.getGame
        self = game.self()

        //Use BWTA to analyze map
        //This may take a few minutes if the map is processed first time!
        System.out.println("Analyzing map...")
        BWTA.readMap()
        BWTA.analyze()
        System.out.println("Map data ready")
    }

    override def onFrame(): Unit = {
      //game.setTextSize(10);
      game.drawTextScreen(10, 10, "Playing as " + self.getName + " - " + self.getRace)

      import scala.collection.JavaConverters._

      self.getUnits.asScala
        .filter(_.getType == UnitType.Terran_Command_Center && self.minerals >= 50)
        .foreach(_.train(UnitType.Terran_SCV))

      self.getUnits.asScala
        .filter(_.getType.isWorker)
        .filter(_.isIdle)
        .foreach { worker =>
        val closestMineral = game.neutral.getUnits.asScala
          .filter(_.getType.isMineralField)
          .map(mineral => (mineral.getDistance(worker), mineral))
          .sortBy(_._1)
          .map(_._2)
          .headOption

        closestMineral.foreach(worker.gather)
      }
    }
}
