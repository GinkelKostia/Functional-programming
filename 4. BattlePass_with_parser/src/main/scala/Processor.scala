final case class ProcessorResult(
    account: BattlePassAccount,
    rejected: List[(BattlePassEvent, BattlePassError | ProcessorError)]
)

object Processor:
    def process(battlePass: BattlePass, settings: BattlePassSettings, events: List[BattlePassEvent]): ProcessorResult =
        events.foldLeft(ProcessorResult(BattlePassAccount(), Nil)) { (state, event) =>
            val result: Either[BattlePassError | ProcessorError, BattlePassAccount] = {
                for
                    res <- battlePass.process(state.account, event)
                    _ <- invariant(settings, res)
                yield res
            }

            result match
                case Right(acc)  => state.copy(account = acc)
                case Left(error) => state.copy(rejected = (event, error) :: state.rejected)
        }

    def invariant(settings: BattlePassSettings, acc: BattlePassAccount): Either[ProcessorError, Unit] =
        Either.cond(acc.points >= 0 && acc.points <= settings.maxLevel * settings.pointsPerLevel, (), ProcessorError.LevelOutOfLimit)
