import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BattlePassTests extends AnyFunSuite with Matchers {
  val settings = BattlePassSettings()
  val battlePass = BattlePass()
  val max_points = settings.maxLevel * settings.pointsPerLevel

  test("Check BattlePassSettings initialization") {
    settings.maxLevel shouldBe 150
    settings.pointsPerLevel shouldBe 1500
    settings.pointsPerDailyTask shouldBe 100
    settings.levelForPremium shouldBe 5
    settings.matchOfTheDayMultiplier shouldBe 2
    settings.giveReward(1) shouldBe Reward(1, "Reward for 1 level")
  }

  test("Daily task completed"){
    val res = battlePass.process(BattlePassAccount(), BattlePassEvent.EventDailyTaskCompleted("1"))

    res.map(_.points) shouldBe Right(settings.pointsPerDailyTask)
    res.map(_.doneTasks) shouldBe Right(Set("1"))
  }

  test("Daily task already completed"){
    val res = battlePass.process(BattlePassAccount(), BattlePassEvent.EventDailyTaskCompleted("1"))
    val res1 = res.flatMap(battlePass.process(_, BattlePassEvent.EventDailyTaskCompleted("1")))

    res.map(_.points) shouldBe Right(settings.pointsPerDailyTask)
    res.map(_.doneTasks) shouldBe Right(Set("1"))

    res1 shouldBe Left(BattlePassError.TaskAlreadyCompletedError("1"))
  }

  test("Daily task completed with max level"){
    val res = battlePass.process(BattlePassAccount(points = max_points), BattlePassEvent.EventDailyTaskCompleted("1"))

    res shouldBe Left(BattlePassError.MaxLevelReachedError)
  }

  test("Match of the day won"){
    val res = battlePass.process(BattlePassAccount(), BattlePassEvent.EventMatchOfTheDayWon)

    res.map(_.points) shouldBe Right(settings.pointsPerDailyTask * settings.matchOfTheDayMultiplier)
  }

  test("Match of the day won with max level "){
    val res = battlePass.process(BattlePassAccount(points = settings.maxLevel * settings.pointsPerLevel), BattlePassEvent.EventMatchOfTheDayWon)

    res shouldBe Left(BattlePassError.MaxLevelReachedError)
  }

  test("Premium purchased"){
    val res = battlePass.process(BattlePassAccount(), BattlePassEvent.EventPremiumPurchased)

    res.map(_.points) shouldBe Right(settings.pointsPerLevel * settings.levelForPremium)
    res.map(_.premium) shouldBe Right(true)
    
  }

  test("Premium already purchased"){
    val res = battlePass.process(BattlePassAccount(premium = true), BattlePassEvent.EventPremiumPurchased)

    res shouldBe Left(BattlePassError.PremiumAlreadyPurchased)
  }

  test("Level up"){
    val res = battlePass.process(BattlePassAccount(points = settings.pointsPerLevel - settings.pointsPerDailyTask), BattlePassEvent.EventDailyTaskCompleted("1"))

    res.map(_.points) shouldBe Right(settings.pointsPerLevel)
    res.map(_.unlockedRewards) shouldBe Right(Vector(settings.giveReward(1)))
    res.map(battlePass.level) shouldBe Right(1)
  }

  test("Points for next level"){
    val res = battlePass.pointsForNextLevel(BattlePassAccount(points = max_points))
    val res1 = battlePass.pointsForNextLevel(BattlePassAccount())

    res shouldBe None
    res1 shouldBe Some(settings.pointsPerLevel)
  }

  test("Check max level"){
    val res = battlePass.isNotMaxLevel(BattlePassAccount(points = max_points))
    val res1 = battlePass.isNotMaxLevel(BattlePassAccount())

    res shouldBe Left(BattlePassError.MaxLevelReachedError)
    res1 shouldBe Right(())
  }

  test("Check is task done"){
    val res = battlePass.isNotTaskDone(BattlePassAccount(doneTasks = Set("1")), "1")
    val res1 = battlePass.isNotTaskDone(BattlePassAccount(), "1")

    res shouldBe Left(BattlePassError.TaskAlreadyCompletedError("1"))
    res1 shouldBe Right(())
  }

  test("Check if premium already bought"){
    val res = battlePass.isNotPremium(BattlePassAccount(premium = true))
    val res1 = battlePass.isNotPremium(BattlePassAccount())

    res shouldBe Left(BattlePassError.PremiumAlreadyPurchased)
    res1 shouldBe Right(())
  }
}