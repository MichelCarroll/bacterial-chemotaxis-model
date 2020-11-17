import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData

import scala.collection.mutable
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ColonySimulation")
object ColonySimulation {

  import DifferentialEquations._

  val width = 1500

  val petriDishHeight = 1000
  val levelViewerHeight = 500

  val delta = 0.01
  val totalTimeSteps = 100000

  val showYLevel = false

  val constants = Constants(
    A_max = 1,
    Y_max = 1,
    B_max = 1,
    R = 1,
    M_max = 1,
    C_max = 1,
    P_max = 1,

    alpha_A = 10,
    alpha_Y = 10,
    alpha_Z = 10,
    alpha_B = 1,
    beta = 0.5,
    alpha_M = 0.5,
    alpha_R = 0.1,
    alpha_1 = 100,
    alpha_2 = 100,

    K_A = 0.5,
    K_Y = 0.5,
    K_Z = 0.5,
    K_B = 0.5,
    K_C = 0.5
  )


  @JSExport
  def start(canvas: Canvas): Unit = {

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.canvas.width = width
    ctx.canvas.height = petriDishHeight + levelViewerHeight

    def inputsAt(x: Double, y: Double): EnvironmentalInputs =
      EnvironmentalInputs(constants.C_max - Math.sqrt(Math.pow(x - width.toDouble / 2, 2) + Math.pow(y - petriDishHeight.toDouble / 2, 2)) * constants.C_max / (petriDishHeight.toDouble / 2))

    val nutrientBackground: ImageData = {
      val data = ctx.createImageData(width, petriDishHeight)
      def indexAt(x: Int, y: Int): Int =
        (y * width + x) * 4

      for {
        x <- 0 until width
        y <- 0 until petriDishHeight
        pixelIndex = indexAt(x, y)
      } {
        val level = ((1 - inputsAt(x, y).C) * 200 + 50).toInt
        data.data.update(pixelIndex+0, level)
        data.data.update(pixelIndex+1, level)
        data.data.update(pixelIndex+2, level)
        data.data.update(pixelIndex+3, 255)
      }
      data
    }



    def clear(): Unit = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, width, petriDishHeight + levelViewerHeight)
    }

    def drawBacteria(bacteria: Bacteria): Unit = {
      ctx.fillStyle = "red"
      val bacteriaWidth: Double = 5.0
      val bacteriaLength: Double = 10.0

      ctx.translate(bacteria.x, petriDishHeight - bacteria.y)
      ctx.rotate(-(bacteria.radians + Math.PI / 2))
      ctx.fillRect(
        x = -bacteriaWidth / 2,
        y = -bacteriaLength / 2,
        w = bacteriaWidth,
        h = bacteriaLength
      )
      ctx.rotate(bacteria.radians + Math.PI / 2)
      ctx.translate(-bacteria.x, bacteria.y - petriDishHeight)
    }

    def drawNutrients(): Unit = {
      ctx.putImageData(nutrientBackground, 0, 0)
    }

    var bacteriaModel = Bacteria(
      x = width.toDouble / 4,
      y = petriDishHeight.toDouble / 2,
      radians = Math.PI / 2,
      levels = Levels(
        A = 0,
        Y = 0.151,
        B = 0,
        MMe = 0,
        MMeActive = 0
      ),
      currentlyTumbling = false,
      timeUntilTumbleDone = 0,
      constants = constants
    )

    bacteriaModel = bacteriaModel.steadyState(inputsAt(bacteriaModel.x, bacteriaModel.y), delta)

    val n = 1000

    var colony: Array[Bacteria] = new Array[Bacteria](n)
    colony.indices.foreach(colony.update(_, bacteriaModel.copy(
      x = width.toDouble * Math.random(),
      y = petriDishHeight.toDouble * Math.random(),
      radians = Math.PI * 2 * Math.random(),
    )))

    def draw(): Unit = {
      drawNutrients()
      colony.foreach { bacteria =>
        drawBacteria(bacteria)
      }
    }

    def updateColony() = {
      colony = colony.map(b => b.updated(inputsAt(b.x, b.y), delta, width, petriDishHeight))
    }

    val levelsHistory = mutable.Queue[Levels]()
    val levelsHistoryMax = (width.toDouble / 2).toInt

    def updateHistory() = {
      levelsHistory.enqueue(colony(0).levels)
      if(levelsHistory.size > levelsHistoryMax)
        levelsHistory.dequeue()
    }

    def drawHistory(min: Double, max: Double) = {
      levelsHistory.zipWithIndex.foreach { case (level, i) =>
        ctx.fillStyle = "white"
        ctx.beginPath()
        val effectiveValue = (level.Y - min) / (max - min)
        ctx.arc(i, petriDishHeight + levelViewerHeight - effectiveValue * levelViewerHeight, 1, 0, Math.PI * 2)
        ctx.fill()
      }
    }

    dom.window.setInterval(() => {
      clear()
      updateColony()
      draw()
      if(showYLevel) {
        updateHistory()
        drawHistory(0.140, 0.160)
      }
    }, 10)



  }


}