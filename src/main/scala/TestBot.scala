import bwapi.{ Unit => ScUnit, _ }
import bwta.BWTA
import rx.lang.scala._

object TestBot {
  def main(args: Array[String]) =
    new TestBot().run()
}

case class GameEvents() {

  private val _unitCreated = Subject[ScUnit]()
  def unitCreated: Observable[ScUnit] = _unitCreated

  private val _frame = Subject[Unit]()
  def frame: Observable[Unit] = _frame

  private val _unitShown = Subject[ScUnit]()
  def unitShown: Observable[ScUnit] = _unitShown

  private val _unitDiscovered = Subject[ScUnit]()
  def unitDiscovered: Observable[ScUnit] = _unitDiscovered

  private val _unitCompleted = Subject[ScUnit]()
  def unitCompleted: Observable[ScUnit] = _unitCompleted

  private val _unitEvaded = Subject[ScUnit]()
  def unitEvaded: Observable[ScUnit] = _unitEvaded

  private val _sentText = Subject[String]()
  def sentText: Observable[String] = _sentText

  private val _end = Subject[Boolean]()
  def end: Observable[Boolean] = _end

  private val _savedGame = Subject[String]()
  def savedGame: Observable[String] = _savedGame

  private val _playerDropped = Subject[Player]()
  def playerDropped: Observable[Player] = _playerDropped

  private val _unitHid = Subject[ScUnit]()
  def unitHid: Observable[ScUnit] = _unitHid

  private val _unitRenegade = Subject[ScUnit]()
  def unitRenegade: Observable[ScUnit] = _unitRenegade

  private val _started = Subject[Unit]()
  def started: Observable[Unit] = _started

  private val _playerLeft = Subject[Player]()
  def playerLeft: Observable[Player] = _playerLeft

  private val _nukeDetected = Subject[Position]()
  def nukeDetected: Observable[Position] = _nukeDetected

  private val _unitDestroyed = Subject[ScUnit]()
  def unitDestroyed: Observable[ScUnit] = _unitDestroyed

  private val _unitMorphed = Subject[ScUnit]()
  def unitMorphed: Observable[ScUnit] = _unitMorphed

  private val _receivedText = Subject[(Player, String)]()
  def receivedText: Observable[(Player, String)] = _receivedText

  val listener = new BWEventListener {
    override def onUnitCreate(unit: ScUnit): Unit = _unitCreated.onNext(unit)
    override def onFrame(): Unit = _frame.onNext()
    override def onUnitShow(unit: ScUnit): Unit = _unitShown.onNext(unit)
    override def onUnitDiscover(unit: ScUnit): Unit = _unitDiscovered.onNext(unit)
    override def onUnitComplete(unit: ScUnit): Unit = _unitCompleted.onNext(unit)
    override def onUnitEvade(unit: ScUnit): Unit = _unitEvaded.onNext(unit)
    override def onSendText(s: String): Unit = _sentText.onNext(s)
    override def onEnd(b: Boolean): Unit = _end.onNext(b)
    override def onSaveGame(s: String): Unit = _savedGame.onNext(s)
    override def onPlayerDropped(player: Player): Unit = _playerDropped.onNext(player)
    override def onUnitHide(unit: ScUnit): Unit = _unitHid.onNext(unit)
    override def onUnitRenegade(unit: ScUnit): Unit = _unitRenegade.onNext(unit)
    override def onStart(): Unit = _started.onNext()
    override def onPlayerLeft(player: Player): Unit = _playerLeft.onNext(player)
    override def onNukeDetect(position: Position): Unit = _nukeDetected.onNext(position)
    override def onUnitDestroy(unit: ScUnit): Unit = _unitDestroyed.onNext(unit)
    override def onUnitMorph(unit: ScUnit): Unit = _unitMorphed.onNext(unit)
    override def onReceiveText(player: Player, s: String): Unit = _receivedText.onNext((player, s))
  }
}

class TestBot {
  val mirror = new Mirror()
  var game: Game = _
  var self: Player = _
  val gameEvents: GameEvents = GameEvents()

  def run(): Unit = {
    mirror.getModule().setEventListener(gameEvents.listener)
    mirror.startGame()
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
      .foreach { worker ?
        val closestMineral = game.neutral.getUnits.asScala
          .filter(_.getType.isMineralField)
          .map(mineral ? (mineral.getDistance(worker), mineral))
          .sortBy(_._1)
          .map(_._2)
          .headOption

        closestMineral.foreach(worker.gather)
      }
  }
}
