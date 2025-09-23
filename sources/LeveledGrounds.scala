package user.sjrd.leveledground

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object LeveledGrounds extends Module

@definition def leveledGroundCreator(using Universe) = new LeveledGroundCreator

final case class ClimbLevelUp(levelDiff: Int) extends Ability
final case class FallLevelDown(levelDiff: Int) extends Ability

final class LeveledGroundCreator(using ComponentInit) extends ComponentCreator[LeveledGround]:
  category = ComponentCategory("leveledgrounds", "Leveled Grounds")

  icon += "Creators/LeveledGroundCreator"
  icon += "Creators/Creator"
end LeveledGroundCreator

class LeveledGround(using ComponentInit) extends Field:
  painter += "Fields/Grass"
  category = ComponentCategory("leveledgrounds", "Leveled Grounds")

  var level: Int = 0

  override def entering(context: MoveContext): Unit = {
    import context.*

    if isRegular then
      val srcPos = src.get.pos
      val destPos = dest.get.pos

      if srcPos.z > destPos.z then
        // ok
        ()
      else if srcPos.z < destPos.z then
        // never ok
        cancel()
      else
        val sourceLevel = src.get().field match
          case leveled: LeveledGround => leveled.level
          case _                        => 0
        val levelDiff = level - sourceLevel
        if levelDiff > 0 then
          if player cannot ClimbLevelUp(levelDiff) then
            cancel()
        else if levelDiff < 0 then
          if player cannot FallLevelDown(-levelDiff) then
            player.showMessage("C'est trop haut pour sauter ici !")
            cancel()
        else
          // ok
          ()
    end if
  }

  override def dispatch[A]: PartialFunction[SquareMessage[A], A] = {
    case PlankInteraction(PlankInteraction.Kind.PassOver, _, passOverPos, leaveFrom, _) =>
      leaveFrom().field match
        case src: LeveledGround =>
          if passOverPos().obstacle == noObstacle then
            src.level > level
          else
            src.level > level + 1
        case _ =>
          false

    case PlankInteraction(PlankInteraction.Kind.LeaveFrom, _, _, leaveFrom, arriveAt) =>
      arriveAt().field match
        case dest: LeveledGround =>
          dest.level == this.level
            && leaveFrom().obstacle == noObstacle
            && arriveAt().obstacle == noObstacle
        case _ =>
          false
  }
end LeveledGround
