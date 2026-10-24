package runner.bots

import runner.*
import scala.util.Random

/**
 * Рефлексивний бот (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на чисті патерн-матчинги в Лекції 2.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero
    val slice: TunnelSlice = observation.upcoming.head
    val lane: Lane = hero.lane

    val lowHeight = slice.hasObstacleAt(lane, Height.Low)
    val midHeight = slice.hasObstacleAt(lane, Height.Mid)
    val highHeight = slice.hasObstacleAt(lane, Height.High)

    if !lowHeight && !midHeight then
      Action.KeepRunning
    else if !lowHeight then
      Action.Duck
    else if !highHeight then
      Action.Jump
    else
      changeLane(slice, lane)

  private def isFreeLane(slice: TunnelSlice, lane: Lane, isHigh: Boolean): Boolean =
      if slice.hasObstacleAt(lane, Height.Low) then return false
      if slice.hasObstacleAt(lane, Height.Mid) then return false
      if isHigh && slice.hasObstacleAt(lane, Height.High) then return false

      return true

  private def changeLane(slice: TunnelSlice, lane: Lane): Action =
      val leftLane = lane.left
      val rightLane = lane.right

      if lane != Lane.Left && leftLane.exists(isFreeLane(slice, _, true)) then return Action.MoveLeft
      if lane != Lane.Right && rightLane.exists(isFreeLane(slice, _, true)) then return Action.MoveRight

      if lane != Lane.Left && leftLane.exists(isFreeLane(slice, _, false)) then return Action.MoveLeft
      if lane != Lane.Right && rightLane.exists(isFreeLane(slice, _, false)) then return Action.MoveRight

      return Action.KeepRunning
