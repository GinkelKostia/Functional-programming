
class BattlePass(settings: BattlePassSettings = BattlePassSettings())
{
    def level(acc: BattlePassAccount): Int = 
        math.min(settings.maxLevel, acc.points / settings.pointsPerLevel)

    def pointsForNextLevel(acc: BattlePassAccount): Option[Int] =
        val lvl = level(acc)
        if lvl >= settings.maxLevel then None
        else Some((lvl + 1) * settings.pointsPerLevel - acc.points)

    def getUnlockedRewards(acc: BattlePassAccount): Vector[Reward] = acc.unlockedRewards

    def isNotMaxLevel(acc: BattlePassAccount): Either[BattlePassError, Unit] =
        Either.cond(level(acc) < settings.maxLevel, (), BattlePassError.MaxLevelReachedError)

    def isNotTaskDone(acc: BattlePassAccount, taskId: String): Either[BattlePassError, Unit] =
        Either.cond(!acc.doneTasks.contains(taskId), (), BattlePassError.TaskAlreadyCompletedError(taskId))

    def isNotPremium(acc: BattlePassAccount): Either[BattlePassError, Unit] =
        Either.cond(!acc.premium, (), BattlePassError.PremiumAlreadyPurchased)

    def process(acc: BattlePassAccount, event: BattlePassEvent): Either[BattlePassError, BattlePassAccount] =
        event match 
        {
            case BattlePassEvent.EventDailyTaskCompleted(taskId) =>
            {
                for
                    _ <- isNotMaxLevel(acc)
                    _ <- isNotTaskDone(acc, taskId) 
                yield award(acc.copy(doneTasks = acc.doneTasks + taskId), settings.pointsPerDailyTask, event)
            }
            case BattlePassEvent.EventMatchOfTheDayWon =>
            {
                isNotMaxLevel(acc).map(_ => award(acc, settings.pointsPerDailyTask * settings.matchOfTheDayMultiplier, event))
            }
            case BattlePassEvent.EventPremiumPurchased =>
            {
                isNotPremium(acc).map(_ => award(acc.copy(premium = true), settings.levelForPremium * settings.pointsPerLevel, event))
            }
            case _ =>
            {
                Left(BattlePassError.InvalidEvent)
            }
        }

    def award(acc: BattlePassAccount, pointsReward: Int, event: BattlePassEvent): BattlePassAccount =
        val oldLevel = level(acc)
        val acc_temp = acc.copy(points = math.min(settings.maxLevel * settings.pointsPerLevel, acc.points + pointsReward))
        val upcomingRewards: IndexedSeq[BattlePassEvent.EventLevelUnlocked] = (oldLevel + 1 to level(acc_temp)).map(level => BattlePassEvent.EventLevelUnlocked(level, settings.giveReward(level)))
        acc_temp.copy(
            unlockedRewards = acc.unlockedRewards ++ upcomingRewards.map(_.reward),
        )   
}