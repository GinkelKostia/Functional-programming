case class Reward(level: Int, name: String)

enum BattlePassEvent:
    case EventDailyTaskCompleted(taskId: String)
    case EventLevelUnlocked(level: Int, reward: Reward)
    case EventMatchOfTheDayWon
    case EventPremiumPurchased

enum BattlePassError:
    case TaskAlreadyCompletedError(taskId: String)
    case MaxLevelReachedError
    case PremiumAlreadyPurchased
    case InvalidEvent

type RewardGiving = Int => Reward
case class BattlePassSettings
(
    maxLevel: Int = 150,
    pointsPerLevel: Int = 1500,
    pointsPerDailyTask: Int = 100,
    levelForPremium: Int = 5,
    matchOfTheDayMultiplier: Int = 2,
    giveReward: RewardGiving = lvl => Reward(lvl, s"Reward for $lvl level")
)
{
    require(maxLevel > 0 && pointsPerLevel > 0 && pointsPerDailyTask > 0)
}

case class BattlePassAccount
(
    points: Int = 0,
    premium: Boolean = false,
    doneTasks: Set[String] = Set.empty,
    unlockedRewards: Vector[Reward] = Vector.empty,
)