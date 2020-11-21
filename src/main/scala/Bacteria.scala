import DifferentialEquations.{Constants, EnvironmentalInputs, Levels, nextDataPoint, rates}

object Bacteria {

  def steadyState(environmentalInputs: EnvironmentalInputs, delta: Double, x: Double, y: Double, radians: Double, levels: Levels, constants: Constants): Bacteria = {
    var currentLevels = levels
    (0 to 10000).foreach { _ =>
      val currentRate = rates(environmentalInputs, constants, currentLevels)
      currentLevels = nextDataPoint(currentLevels, currentRate, delta)
    }

    Bacteria(x = x, y = y, radians = radians, levels = currentLevels, currentlyTumbling = false, constants = constants, timeUntilTumbleDone = 0, steadyStateYLevel = currentLevels.Y)
  }

}

case class Bacteria(x: Double, y: Double, radians: Double, levels: Levels, currentlyTumbling: Boolean, constants: Constants, timeUntilTumbleDone: Int, steadyStateYLevel: Double) {

  val bacteriaRadiansPerTime = Math.PI / 5
  val bacteriaMoveSpeed = 0.4
  val maxTumbleTime = 10

  def clamp(value: Double, max: Double): Double = if(value >= 0) value % max else value + max

  def probabilityOfTumbling: Double = {
    val x = levels.Y - steadyStateYLevel
    val b = -45
    val a = -0.1
    1 / (1 + Math.pow(Math.E, b * (x + a)))
  }

  def steadyState(environmentalInputs: EnvironmentalInputs, delta: Double): Bacteria = {
    var currentLevels = levels
    (0 to 10000).foreach { _ =>
      val currentRate = rates(environmentalInputs, constants, currentLevels)
      currentLevels = nextDataPoint(currentLevels, currentRate, delta)
    }
    copy(levels = currentLevels)
  }

  def updated(environmentalInputs: EnvironmentalInputs, delta: Double, maxX: Double, maxY: Double): Bacteria = {

    val currentRate = rates(environmentalInputs, constants, levels)
    val newLevels = nextDataPoint(levels, currentRate, delta)

    var newCurrentlyTumbling = currentlyTumbling
    var newTimeUntilTumbleDone = timeUntilTumbleDone

    if(currentlyTumbling) {
      newTimeUntilTumbleDone = timeUntilTumbleDone - 1
      if(timeUntilTumbleDone < 0) {
        newCurrentlyTumbling = false
      }
    }
    else {
      if(Math.random() < probabilityOfTumbling) {
        newCurrentlyTumbling = true
        newTimeUntilTumbleDone = (Math.random() * maxTumbleTime).toInt
      }
    }

    val intermediateBacteria = copy(
      levels = newLevels,
      currentlyTumbling = newCurrentlyTumbling,
      timeUntilTumbleDone = newTimeUntilTumbleDone
    )

    if(currentlyTumbling) {
      intermediateBacteria.copy(
        radians = radians + bacteriaRadiansPerTime
      )
    } else {
      intermediateBacteria.copy(
        x = clamp(x + Math.cos(radians) * bacteriaMoveSpeed, maxX),
        y = clamp(y + Math.sin(radians) * bacteriaMoveSpeed, maxY),
      )
    }
  }

}