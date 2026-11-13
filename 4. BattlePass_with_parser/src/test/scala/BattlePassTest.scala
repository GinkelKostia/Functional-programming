import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.Using

class BattlePassTests extends AnyFunSuite with Matchers {
  val settings = BattlePassSettings()
  val battlePass = BattlePass()
  val max_points = settings.maxLevel * settings.pointsPerLevel

  def run(lines: List[String]): (List[ParserError], ProcessorResult) = {
    val (errs, events) = Parser.parseBatch(lines)
    (errs, Processor.process(battlePass, settings, events))
  }

  test("BattlePassSettings: initialization") {
    settings.maxLevel shouldBe 100
    settings.pointsPerLevel shouldBe 1500
    settings.pointsPerTaskMin shouldBe 120
    settings.pointsPerTaskMax shouldBe 750
    settings.levelForPremium shouldBe 5
    settings.matchWinMultiplier shouldBe 2
  }

  test("BattlePassAccount: initialization") {
    val acc = BattlePassAccount()
    acc.points shouldBe 0
    acc.premium shouldBe false
    acc.unlockedRewards shouldBe empty
  }

  test("Smart constructor: Points") {
    Points.fromString("302", settings.pointsPerTaskMin, settings.pointsPerTaskMax).map(_.points) shouldBe Right(302)
    Points.fromString(" 302 ", settings.pointsPerTaskMin, settings.pointsPerTaskMax).map(_.points) shouldBe Right(302)
    Points.fromString("xx", settings.pointsPerTaskMin, settings.pointsPerTaskMax) shouldBe Left("xx")
    Points.fromString("-5", settings.pointsPerTaskMin, settings.pointsPerTaskMax) shouldBe Left("-5")
    Points.fromString("0", settings.pointsPerTaskMin, settings.pointsPerTaskMax) shouldBe Left("0")
    Points.fromStringPremium("5", settings.pointsPerLevel, settings.levelForPremium).map(_.points) shouldBe Right(7500)
    Points.fromStringPremium("xx", settings.pointsPerLevel, settings.levelForPremium) shouldBe Left("xx")
  }

  test("Smart constructor: Tier") {
    Tier.fromString("1", settings.maxLevel).map(_.tier) shouldBe Right(1)
    Tier.fromString("100", settings.maxLevel).map(_.tier) shouldBe Right(100)
    Tier.fromString("0", settings.maxLevel) shouldBe Left("0")
    Tier.fromString("151", settings.maxLevel) shouldBe Left("151")
    Tier.fromString("abc", settings.maxLevel) shouldBe Left("abc")
  }

  test("Parser: valid lines") {
    Parser.parseLine(1, "DAILY_QUEST,302,,,").isRight shouldBe true
    Parser.parseLine(2, "MATCH_WIN,1186,,,").isRight shouldBe true
    Parser.parseLine(3, "CLAIM_TIER,3,,,").isRight shouldBe true
    Parser.parseLine(4, "PREMIUM_BOOST,5,,,").isRight shouldBe true
  }

  test("Parser: invalid lines") {
    Parser.parseLine(1, "MATCH_WIN,xx,,,") shouldBe Left(ParserError.InvalidField)
    Parser.parseLine(2, "CLAIM_TIER,999,,,") shouldBe Left(ParserError.InvalidField)
    Parser.parseLine(3, "DAILY_QUEST,-10,,,") shouldBe Left(ParserError.InvalidField)
    Parser.parseLine(4, "DAILY_QUEST") shouldBe Left(ParserError.InvalidRowFormat)
    Parser.parseLine(5, "a,b,c,d,e,f") shouldBe Left(ParserError.InvalidRowFormat)
    Parser.parseLine(6, "event,p1,p2,p3,p4") shouldBe Left(ParserError.UnknownEvent)
    Parser.parseLine(7, "FOO,1,,,") shouldBe Left(ParserError.UnknownEvent)
  }

  test("Points accumulate and level is calculated") {
    val (errs, result) = run(List("DAILY_QUEST,300,,,", "MATCH_WIN,1100,,,"))
    errs shouldBe empty
    result.account.points shouldBe 1400
    battlePass.level(result.account) shouldBe 0
    battlePass.pointsForNextLevel(result.account) shouldBe Some(100)
  }

  test("Premium boost gives levels and enables premium") {
    val (_, result) = run(List("PREMIUM_BOOST,5,,,"))
    result.account.points shouldBe 7500
    result.account.premium shouldBe true
    battlePass.level(result.account) shouldBe 5
  }

  test("Points are capped at max") {
    val (_, result) = run(List.fill(101)("MATCH_WIN,1500,,,"))
    result.account.points shouldBe max_points
    battlePass.level(result.account) shouldBe settings.maxLevel
    battlePass.pointsForNextLevel(result.account) shouldBe None
  }

  test("Claim reachable tier") {
    val (_, result) = run(List.fill(4)("DAILY_QUEST,750,,,") :+ "CLAIM_TIER,2,,,")
    result.rejected shouldBe empty
    result.account.unlockedRewards shouldBe Vector(Reward(2))
  }

  test("Claim same tier twice is rejected") {
    val (_, result) = run(List("MATCH_WIN,1500,,,", "CLAIM_TIER,1,,,", "CLAIM_TIER,1,,,"))
    result.rejected.size shouldBe 1
    result.account.unlockedRewards.size shouldBe 1
  }

  test("Invariant true state") {
    val (_, result) = run(List("PREMIUM_BOOST,5,,,", "CLAIM_TIER,1,,,", "CLAIM_TIER,5,,,"))
    Processor.invariant(settings, result.account).isRight shouldBe true
  }

  test("Invariant detects level out of limit") {
    val bad = BattlePassAccount(points = max_points + 1)
    Processor.invariant(settings, bad).isLeft shouldBe true
  }

  test("CSV fragment") {
    val lines = Using.resource(Source.fromFile("resources/variant_16_battle_pass.csv"))(_.getLines().toList).drop(1)
    val (errs, result) = run(lines)
    errs should not be empty
    errs.size should be < lines.size
    (lines.size - errs.size) shouldBe 475098
    Processor.invariant(settings, result.account) shouldBe Right(())
  }
}