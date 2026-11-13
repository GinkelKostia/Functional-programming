object Parser:
    private val settings = BattlePassSettings()

    def parseBatch(lines: List[String]): (List[ParserError], List[BattlePassEvent]) =
        lines.zipWithIndex.partitionMap((line, idx) => parseLine(idx + 1, line))

    def parseLine(row: Int, line: String): Either[ParserError, BattlePassEvent] =
        val data = line.split(",", -1)
        if data.length != 5 then Left(ParserError.InvalidRowFormat)
        else
            data(0).trim match
                case "DAILY_QUEST" =>
                    Points.fromString(data(1), settings.pointsPerTaskMin, settings.pointsPerTaskMax).left.map(_ => ParserError.InvalidField).map(BattlePassEvent.DailyQuestEvent(_))
                case "MATCH_WIN" =>
                    Points.fromString(data(1), settings.pointsPerTaskMin, settings.pointsPerTaskMax, settings.matchWinMultiplier).left.map(_ => ParserError.InvalidField).map(BattlePassEvent.MatchWinEvent(_))
                case "CLAIM_TIER" =>
                    Tier.fromString(data(1), settings.maxLevel).left.map(_ => ParserError.InvalidField).map(BattlePassEvent.ClaimTierEvent(_))
                case "PREMIUM_BOOST" =>
                    Points.fromStringPremium(data(1), settings.pointsPerLevel, settings.levelForPremium).left.map(_ => ParserError.InvalidField).map(BattlePassEvent.PremiumBoostEvent(_))
                case _ => Left(ParserError.UnknownEvent)