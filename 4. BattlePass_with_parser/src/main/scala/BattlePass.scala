class BattlePass(settings: BattlePassSettings = BattlePassSettings())
{
    def level(acc: BattlePassAccount): Int =
        math.min(settings.maxLevel, acc.points / settings.pointsPerLevel)

    def pointsForNextLevel(acc: BattlePassAccount): Option[Int] =
        val lvl = level(acc)
        if lvl >= settings.maxLevel then None
        else Some((lvl + 1) * settings.pointsPerLevel - acc.points)

    def getUnlockedRewards(acc: BattlePassAccount): Vector[Reward] = acc.unlockedRewards

    def isTierReached(acc: BattlePassAccount, t: Tier): Either[BattlePassError, Unit] =
        Either.cond(t.tier <= level(acc), (), BattlePassError.TierNotReached)

    def isTierRewardNotClaimed(acc: BattlePassAccount, t: Tier): Either[BattlePassError, Unit] =
        Either.cond(!acc.unlockedRewards.exists(_.level == t.tier), (), BattlePassError.TierRewardAlreadyClaimed)

    def addPoints(acc: BattlePassAccount, xp: Points): BattlePassAccount =
        acc.copy(points = math.min(settings.maxLevel * settings.pointsPerLevel, acc.points + xp.points))

    def process(acc: BattlePassAccount, event: BattlePassEvent): Either[BattlePassError, BattlePassAccount] =
        event match
        {
            case BattlePassEvent.DailyQuestEvent(xp)   => Right(addPoints(acc, xp))
            case BattlePassEvent.MatchWinEvent(xp)     => Right(addPoints(acc, xp))
            case BattlePassEvent.PremiumBoostEvent(xp) => Right(addPoints(acc, xp).copy(premium = true))
            case BattlePassEvent.ClaimTierEvent(tier) =>
                for
                    _ <- isTierReached(acc, tier)
                    _ <- isTierRewardNotClaimed(acc, tier)
                yield acc.copy(unlockedRewards = acc.unlockedRewards :+ Reward(tier.tier))
        }
}