sealed trait Light
case object Green extends Light
case object Red extends Light
case object White extends Light

/**
 * Each Vessel has three different lights on board.[Red, Green, White].
 * Apply function parses a lightMark into a Light object [Red, Green, White].
 * lightMarks are first letter(ignoring case) representations of a color, e.g. r =>
 * Red , R => Red, g => Green ...
 * If the light isn't recognizable, returns None.
 */
object Light {
  def apply(lightMark: String): Option[Light] = {
    lightMark.toLowerCase match {
      case "r" => Some(Red)
      case "g" => Some(Green)
      case "w" => Some(White)
      case _ => None
    }
  }
}

sealed trait Direction
case object Towards extends Direction
case object Away extends Direction
case object Left extends Direction
case object Right extends Direction
case object Unknown extends Direction

/**
 * Determines direction based on the lights noted on a vessel, based on these rules:
 * - Red => The Port side is visible, meaning that the Direction the vessel is
 * heading Left
 * - Green => The Starboard side is visible, meaning that the Direction the vessel is
 * heading Right
 * - Green & Red=> Both Port & Starboard sides are visible, meaning the vessel is
 * heading Towards us(order is irrelevant)
 * - White => We're looking at the aft, meaning the vessel is heading Away
 * - Any other combination means that someone messed up the colours and the heading
 * is Unknown
 */
object Direction {
  def apply(lights: Seq[Light]): Direction = {
    val lightSet = lights.toSet
    if (lightSet == Set(Red, Green)) Towards
    else if (lightSet == Set(White)) Away
    else if (lightSet == Set(Red)) Left
    else if (lightSet == Set(Green)) Right
    else Unknown
  }
}

/**
 * Each vessel has a direction, which is calculated based on its lightMarks
 * The direction should be calculated based on the fully understood Lights. Meaning,
 * if one of the lightMarks is None
 * we cannot determine the direction. If all lights are known, we should be able to
 * identify its direction.
 * @param lightMarks
 */
case class Vessel(lightMarks: Seq[String]) {
  def direction: Direction = {
    val lights = lightMarks.flatMap(Light.apply)
    if (lights.size != lightMarks.size) Unknown
    else Direction(lights)
  }
}

/**
 * Binoculars are our way of looking at things on the horizon.
 * Horizon is defined as a sequence of sequences of light marks.
 * The horizon contains 360 (as in 360 degrees) elements, each containing more or no
 light marks.
 * The center of the binoculars is the direction of where the binoculars are
 pointing, e.g. center = 45, means you're looking North-East
 * The angle defines how much of the horizon is visible through the binoculars. The
 higher the zoom, the lower the angle.
 */
object Binoculars {

  /**
   * Counts vessels on the horizon, given the direction they are pointed to
   *
   * @param lightMarksOnHorizon the entire horizon 0 - 360
   * @param center              - degree number where the binoculars are pointing
   * @param angle               - the zoom level, how many degrees of the horizon are visible
   * @param directionFilter - the filter function which defines whether we're
   *                                                    searching for vessels heading left, right, or ...
   * @return the number of ships visible in the subset of the horizon narrowed by the
   *                  angle, applying the direction filter
   */
  def countVessels(lightMarksOnHorizon: Seq[Seq[String]],
                   center: Int,
                   angle: Int,
                   directionFilter: Vessel => Boolean): Int = {
    val start = (center - angle / 2 + 360) % 360
    val end = (center + angle / 2) % 360
    val range = if (start <= end) start to end else (start until 360) ++ (0 to end)

    range.map(i => lightMarksOnHorizon(i % 360))
      .filter(_.nonEmpty)
      .count(marks => directionFilter(Vessel(marks)))
  }

  /**
   * Finds the degree to which binoculars should be pointed, where most vessels can
   * be seen.
   *
   * @param lightMarksOnHorizon the entire horizon 0 - 360
   * @param angle               - the zoom level, how many degrees of the horizon are visible
   * @return the center of the binoculars where they should be pointed to
  */
  def mostVessels(lightMarksOnHorizon: Seq[Seq[String]],
                  angle: Int): Int = {
    val range = (0 until 360).toList

    // Create a map of centers to their vessel counts
    val vesselCounts = range.map(center => center -> countVessels(lightMarksOnHorizon, center, angle, _ => true)).toMap

    // Find the center with the maximum vessel count
    vesselCounts.maxBy(_._2)._1
  }
}
object Solution extends App {
  case class LightsCase(options: String, result: Option[Light])
  case class DirectionCase(options: Seq[Light], result: Direction)
  case class VesselCase(options: Seq[String], result: Direction)
  def checker() = {
    def seq() = (0 until 360).map(_ => Seq.empty[String]).toList

    def checkLights(assertion: LightsCase, result: Option[Light]): Unit = {
      if (assertion.result == result) {
        println(Console.GREEN + s"${assertion.options} returned $result")
      } else {
        println(Console.RED + s"${assertion.options} returned $result, expected ${assertion.result}")
      }
    }

    def checkDirection(assertion: DirectionCase, result: Direction): Unit = {
      if (assertion.result == result) {
        println(Console.GREEN + s"${assertion.options.mkString} returned $result")
      } else {
        println(Console.RED + s"${assertion.options.mkString} returned $result, expected ${assertion.result}")
      }
    }

    def checkVessel(assertion: VesselCase, result: Direction): Unit = {
      if (assertion.result == result) {
        println(Console.GREEN + s"${assertion.options.mkString} returned $result")
      } else {
        println(Console.RED + s"${assertion.options.mkString} returned $result, expected ${assertion.result}")
      }
    }

    println(Console.WHITE + s"\nChecking Lights....")
    Seq(
      LightsCase("r", Some(Red)), LightsCase("R", Some(Red)),
      LightsCase("w", Some(White)), LightsCase("W", Some(White)),
      LightsCase("g", Some(Green)), LightsCase("G", Some(Green)),
      LightsCase("c", None), LightsCase("235", None), LightsCase("sa24r", None)
    ).foreach(c => checkLights(c, Light.apply(c.options)))

    println(Console.WHITE + s"\nChecking Directions....")
    Seq(
      DirectionCase(Seq(White), Away),
      DirectionCase(Seq(Green, Red), Towards),
      DirectionCase(Seq(Red, Green), Towards),
      DirectionCase(Seq(Green), Right),
      DirectionCase(Seq(Red), Left),
      DirectionCase(Seq(Red, White), Unknown),
      DirectionCase(Seq(White, Red), Unknown),
      DirectionCase(Seq(Green, White), Unknown),
      DirectionCase(Seq(White, Green), Unknown),
      DirectionCase(Seq(Red, White, Green), Unknown),
      DirectionCase(Seq(White, Red, Green), Unknown),
      DirectionCase(Seq(White, Green, Red), Unknown),
      DirectionCase(Seq(Green, White, Red), Unknown),
    ).foreach(c => checkDirection(c, Direction.apply(c.options)))


    println(Console.WHITE + s"\nChecking Vessels....")
    Seq(
      VesselCase(Seq("xsaf", "g"), Unknown),
      VesselCase(Seq("r", "g"), Towards),
      VesselCase(Seq("w", "g"), Unknown),
      VesselCase(Seq("w", "r"), Unknown),
      VesselCase(Seq("g"), Right)
    ).foreach(c => checkVessel(c, Vessel(c.options).direction))

    val horizon = seq()
      .updated(0, Seq("b")).updated(3, Seq("R")).updated(5, Seq("R", "G")).updated(14, Seq("W")).updated(19, Seq("W"))
      .updated(21, Seq("R")).updated(26, Seq("G")).updated(35, Seq("R", "G"))
      .updated(42, Seq("R", "G")).updated(47, Seq("R", "G")).updated(55, Seq("R", "G")).updated(67, Seq("W", "G"))
      .updated(74, Seq("R", "G")).updated(78, Seq("W")).updated(82, Seq("R"))
      .updated(95, Seq("R", "G"))
      .updated(137, Seq("R", "G")).updated(145, Seq("R", "G"))
      .updated(172, Seq("R", "G")).updated(182, Seq("W"))
      .updated(198, Seq("R", "G")).updated(207, Seq("R", "G"))
      .updated(212, Seq("R", "G")).updated(229, Seq("R", "G"))
      .updated(231, Seq("R", "G")).updated(246, Seq("R", "G"))
      .updated(259, Seq("R", "G")).updated(263, Seq("R", "G"))
      .updated(301, Seq("R", "G"))
      .updated(328, Seq("R", "G"))
      .updated(346, Seq("R", "G"))
      .updated(358, Seq("R", "G")).updated(359, Seq("W"))

    println(Console.WHITE + s"\nChecking Binoculars.countVessels(horizon, 0, 30, _ => true)")
    // 0 - 15 = 4 + 345 - 360 = 3 => 7
    val case1 = Binoculars.countVessels(horizon, 0, 30, _ => true)
    if (case1 == 7) {
      println(Console.GREEN + s"Binoculars.countVessels(horizon, 0, 30, _ => true) returned $case1")
    } else {
      println(Console.RED + s"Binoculars.countVessels(horizon, 0, 30, _ => true) returned $case1, expected 7")
    }

    println(Console.WHITE + s"\nChecking Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown)")
    // Just one Seq("b")
    val case11 = Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown)
    if (case11 == 1) {
      println(Console.GREEN + s"Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown) returned $case11")
    } else {
      println(Console.RED + s"Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown) returned $case11, expected 1")
    }

    println(Console.WHITE + s"\nChecking Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown)")
    // Just one Seq("b")
    val case12 = Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Away)
    if (case12 == 2) {
      println(Console.GREEN + s"Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown) returned $case12")
    } else {
      println(Console.RED + s"Binoculars.countVessels(horizon, 0, 30, vessel => vessel.direction == Unknown) returned $case12, expected 2")
    }

    println(Console.WHITE + s"\nChecking Binoculars.countVessels(horizon, 15, 60, _ => true")
    // 15 - 45 = 5 + 345 - 15 = 7 => 12
    val case2 = Binoculars.countVessels(horizon, 15, 60, _ => true)
    if (case2 == 12) {
      println(Console.GREEN + s"Binoculars.countVessels(horizon, 15, 60, _ => true) returned $case2")
    } else {
      println(Console.RED + s"Binoculars.countVessels(horizon, 15, 60, _ => true) returned $case2, expected 12")
    }

    println(Console.WHITE + s"\nBinoculars.countVessels(horizon, 350, 80, _ => true")
    // 350 - 310 = 2 + 350 - 30 = 9 => 11
    val case4 = Binoculars.countVessels(horizon, 350, 80, _ => true)
    if (case4 == 11) {
      println(Console.GREEN + s"Binoculars.countVessels(horizon, 350, 80, _ => true) returned $case4")
    } else {
      println(Console.RED + s"Binoculars.countVessels(horizon, 350, 80, _ => true) returned $case4, expected 11")
    }

    println(Console.WHITE + s"\nBinoculars.mostVessels(horizon, 30)")
    val case5 = Binoculars.mostVessels(horizon, 30)
    // most of them with a 30 angle are between 358 - 28 = 9 , angle should be 13
    if (case5 == 13) {
      println(Console.GREEN + s"Binoculars.mostVessels(horizon, 30) returned $case5")
    } else {
      println(Console.RED + s"Binoculars.mostVessels(horizon, 30) returned $case5, expected 13")
    }

    println(Console.WHITE + s"\nBinoculars.mostVessels(horizon, 20)")
    // most of them with a 20 angle are between 359 - 19 = 6 , angle should be 9
    val case6 = Binoculars.mostVessels(horizon, 20)
    if (Seq(4, 5, 6, 7, 8, 9, 355, 356).contains(case6)) {
      println(Console.GREEN + s"Binoculars.mostVessels(horizon, 20) returned $case6")
    } else {
      println(Console.RED + s"Binoculars.mostVessels(horizon, 20) returned $case6, expected 9")
    }

  }

  checker()
}
