import org.scalajs.dom
import org.scalajs.dom.html.Canvas

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("RobustnessTest")
object RobustnessTest {

  import DifferentialEquations._

  val inputHistory = new ArrayBuffer[EnvironmentalInputs]()
  val dataPointHistory = new ArrayBuffer[Levels]()
  val rateHistory = new ArrayBuffer[Rates]()

  val delta = 0.001
  val totalTimeSteps = 100000

  val constants = Constants(
    A_max = 1,
    Y_max = 1,
    B_max = 1,
    C_max = 1,
    P_max = 1,

    alpha_A = 10,
    alpha_Y = 10,
    alpha_Z = 10,
    alpha_B = 10,
    alpha_Bp = 10,
    alpha_M = 1,
    alpha_R = 0.1,
    alpha_1 = 200,
    alpha_2 = 100,

    K_A = 0.5,
    K_Y = 0.5,
    K_Z = 0.5,
    K_B = 0.5,
    K_C = 0.5
  )

  val initialPoint = Levels(
    A = 0,
    Y = 0,
    B = 0,
    MMe = 0,
    MMeActive = 0
  )
  dataPointHistory.append(initialPoint)

  val initialRates = Rates(0,0,0,0,0)
  rateHistory.append(initialRates)

  inputHistory.append(EnvironmentalInputs(0))

  def testScenario1() = {
    (1 until totalTimeSteps).foreach { t =>
      val lastPoint = dataPointHistory(t-1)
      val timePerc = t.toDouble/totalTimeSteps
      val inputs = timePerc match {
        case z if z < 1.0/4 =>
          EnvironmentalInputs(0.1)
        case z if z < 2.0/4 =>
          EnvironmentalInputs(0.4)
        case z if z < 3.0/4 =>
          EnvironmentalInputs(0.1)
        case _ =>
          EnvironmentalInputs(0.4)
      }
      val currentRate = rates(inputs, constants, lastPoint)
      val currentDataPoint = nextDataPoint(lastPoint, currentRate, delta)

      inputHistory.append(inputs)
      rateHistory.append(currentRate)
      dataPointHistory.append(currentDataPoint)
    }
  }

  def testScenario2() = {
    (1 until totalTimeSteps).foreach { t =>
      val lastPoint = dataPointHistory(t-1)
      val timePerc = t.toDouble/totalTimeSteps
      val inputs = timePerc match {
        case z if z < 1.0/4 =>
          EnvironmentalInputs(0.1)
        case z if z < 2.0/4 =>
          EnvironmentalInputs((timePerc - 0.25) * 4 * 0.4 + 0.1)
        case z if z < 3.0/4 =>
          EnvironmentalInputs(0.5 - (timePerc - 0.5) * 4 * 0.4)
        case _ =>
          EnvironmentalInputs(0.1)
      }
      val currentRate = rates(inputs, constants, lastPoint)
      val currentDataPoint = nextDataPoint(lastPoint, currentRate, delta)

      inputHistory.append(inputs)
      rateHistory.append(currentRate)
      dataPointHistory.append(currentDataPoint)
    }
  }

  testScenario1()

//  println(inputHistory)

  @JSExport
  def start(canvas: Canvas): Unit = {

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    val canvasWidth = 2000
    val canvasHeight = 1000

    ctx.canvas.width = canvasWidth
    ctx.canvas.height = canvasHeight

    def clear(): Unit = {
      ctx.fillStyle = "white"
      ctx.fillRect(0, 0, canvasWidth, canvasHeight)
    }

    def draw(): Unit = {

      dataPointHistory.zipWithIndex.foreach { case (data, t) =>

        val inputs = inputHistory(t)

        def drawPoint(value: Double, max: Double, color: String, width: Int = 1): Unit = {
          val xPos = (t.toDouble / totalTimeSteps) * canvasWidth
          val yPos = canvasHeight - ((value / max) * canvasHeight)
          ctx.fillStyle = color
          ctx.beginPath()
          ctx.arc(xPos, yPos, width, 0, Math.PI)
          ctx.fill()
        }

        import data._
        import constants._
        import inputs._

        drawPoint(A, A_max/2, "red")
        drawPoint(Y, Y_max/2, "black", 2)
        drawPoint(B, B_max/2, "blue")
        drawPoint(MMe, 1.0/2, "green")
        drawPoint(MMeActive, 1.0/2, "cyan")
        drawPoint(C, C_max/2, "orange", 2)
      }
    }

    clear()
    draw()

  }

}

