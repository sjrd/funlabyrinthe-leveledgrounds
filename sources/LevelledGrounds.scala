package user.sjrd.levelledground

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object LevelledGrounds extends Module:
  override protected def createComponents()(using Universe): Unit =
    val levelledGroundCreator = new LevelledGroundCreator
  end createComponents
  
  def levelledGroundCreator(using Universe): LevelledGroundCreator =
    myComponentByID("levelledGroundCreator")
end LevelledGrounds

export LevelledGrounds.*

final case class ClimbLevelUp(levelDiff: Int) extends Ability
final case class FallLevelDown(levelDiff: Int) extends Ability

final class LevelledGroundCreator(using ComponentInit) extends ComponentCreator[LevelledGround]:
  category = ComponentCategory("levelledgrounds", "Levelled Grounds")

  icon += "Creators/LevelledGroundCreator"
  icon += "Creators/Creator"
end LevelledGroundCreator
  
class LevelledGround(using ComponentInit) extends Field:
  painter += "Fields/Grass"
  category = ComponentCategory("levelledgrounds", "Levelled Grounds")

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
          case levelled: LevelledGround => levelled.level
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
        case src: LevelledGround => 
          if passOverPos().obstacle == noObstacle then
            src.level > level
          else
            src.level > level + 1
        case _ =>
          false

    case PlankInteraction(PlankInteraction.Kind.LeaveFrom, _, _, leaveFrom, arriveAt) =>
      arriveAt().field match
        case dest: LevelledGround =>
          dest.level == this.level
            && leaveFrom().obstacle == noObstacle
            && arriveAt().obstacle == noObstacle
        case _ =>
          false
  }
end LevelledGround