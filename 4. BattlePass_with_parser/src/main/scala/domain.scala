final case class Reward(level: Int)

enum BattlePassEvent:
    case DailyQuestEvent(points: Points)
    case ClaimTierEvent(tier: Tier)
    case MatchWinEvent(points: Points)
    case PremiumBoostEvent(points: Points)

enum BattlePassError:
    case TierNotReached
    case TierRewardAlreadyClaimed
    case InvalidEvent

enum ParserError:
    case InvalidRowFormat
    case UnknownEvent
    case InvalidField

enum ProcessorError:
    case LevelOutOfLimit

final case class Points private (points: Int);
object Points:
    def fromString(data: String, min: Int, max: Int, multiplier: Int = 1): Either[String, Points] = 
        data.trim.toIntOption.filter(points => points >= min * multiplier && points <= max * multiplier).map(new Points(_)).toRight(data)
    def fromStringPremium(data: String, pointsPerLevel: Int, premiumLevelBonus: Int): Either[String, Points] = 
        data.trim.toIntOption.filter(_ == premiumLevelBonus).map(levels => new Points(levels * pointsPerLevel)).toRight(data)

final case class Tier private (tier: Int);
object Tier:
    def fromString(data: String, maxTier: Int): Either[String, Tier] = 
        data.trim.toIntOption.filter(t => t >= 1 && t <= maxTier).map(new Tier(_)).toRight(data)

case class BattlePassSettings
(
    maxLevel: Int = 100,
    pointsPerLevel: Int = 1500,
    pointsPerTaskMin: Int = 120,
    pointsPerTaskMax: Int = 750,
    levelForPremium: Int = 5,
    matchWinMultiplier: Int = 2,
)
{
    require(maxLevel > 0 && pointsPerLevel > 0 && pointsPerTaskMin > 0 && pointsPerTaskMax > 0)
}

case class BattlePassAccount
(
    points: Int = 0,
    premium: Boolean = false,
    unlockedRewards: Vector[Reward] = Vector.empty,
)