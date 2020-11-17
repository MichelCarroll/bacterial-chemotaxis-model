import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData

import scala.collection.mutable
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Random

@JSExportTopLevel("EvolutionSimulation")
object EvolutionSimulation {

  import DifferentialEquations._

  val width = 1500

  val height = 1000

  val delta = 0.01
  val totalTimeSteps = 100000

  val initialConstants = Constants(
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

  def mutated(base: Constants): Constants = {
    def normalWithMean(base: Double): Double = Random.nextGaussian() * base / 4 + base //std dev of 1/4 the value
    import base._
    Constants(
      A_max = normalWithMean(A_max),
      Y_max = normalWithMean(Y_max),
      B_max = normalWithMean(B_max),
      R =     normalWithMean(R),
      M_max = normalWithMean(M_max),
      C_max = C_max, //not randomizable
      P_max = normalWithMean(P_max),

      alpha_A = normalWithMean(alpha_A),
      alpha_Y = normalWithMean(alpha_Y),
      alpha_Z = normalWithMean(alpha_Z),
      alpha_B = normalWithMean(alpha_B),
      beta =    normalWithMean(beta),
      alpha_M = normalWithMean(alpha_M),
      alpha_R = normalWithMean(alpha_R),
      alpha_1 = normalWithMean(alpha_1),
      alpha_2 = normalWithMean(alpha_2),

      K_A = normalWithMean(K_A),
      K_Y = normalWithMean(K_Y),
      K_Z = normalWithMean(K_Z),
      K_B = normalWithMean(K_B),
      K_C = normalWithMean(K_C)
    )
  }


  val bacteriaRadiansPerTime = Math.PI / 5
  val bacteriaMoveSpeed = 1
  val maxTumbleTime = 10


  @JSExport
  def start(canvas: Canvas): Unit = {

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.canvas.width = width
    ctx.canvas.height = height

    def inputsAt(x: Double, y: Double): EnvironmentalInputs =
      EnvironmentalInputs(initialConstants.C_max - Math.sqrt(Math.pow(x - width.toDouble / 2, 2) + Math.pow(y - height.toDouble / 2, 2)) * initialConstants.C_max / (height.toDouble / 2))

    val nutrientBackground: ImageData = {
      val data = ctx.createImageData(width, height)
      def indexAt(x: Int, y: Int): Int =
        (y * width + x) * 4

      for {
        x <- 0 until width
        y <- 0 until height
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
      ctx.fillRect(0, 0, width, height)
    }

    def drawBacteria(bacteria: Bacteria): Unit = {
      ctx.fillStyle = "red"
      val bacteriaWidth: Double = 5.0
      val bacteriaLength: Double = 10.0

      ctx.translate(bacteria.x, height - bacteria.y)
      ctx.rotate(-(bacteria.radians + Math.PI / 2))
      ctx.fillRect(
        x = -bacteriaWidth / 2,
        y = -bacteriaLength / 2,
        w = bacteriaWidth,
        h = bacteriaLength
      )
      ctx.rotate(bacteria.radians + Math.PI / 2)
      ctx.translate(-bacteria.x, bacteria.y - height)
    }

    def drawNutrients(): Unit = {
      ctx.putImageData(nutrientBackground, 0, 0)
    }

    var bacteriaModel = Bacteria(
      x = width.toDouble * Math.random(),
      y = height.toDouble * Math.random(),
      radians = Math.PI * 2 * Math.random(),
      levels = Levels(
        A = 0,
        Y = 0.151,
        B = 0,
        MMe = 0,
        MMeActive = 0
      ),
      currentlyTumbling = false,
      timeUntilTumbleDone = 0,
      constants = initialConstants
    )

    bacteriaModel = bacteriaModel.steadyState(inputsAt(bacteriaModel.x, bacteriaModel.y), delta)

    val n = 1000
    val winnerN = 100
    val cloneFactor = 10
    val maxT = 4000
    var tUntilFindWinner = 0

    var initialNutrients: Array[Double] = new Array[Double](n)

    var colony: Array[Bacteria] = new Array[Bacteria](n)
    colony.indices.foreach(colony.update(_, bacteriaModel.copy(
      constants = mutated(bacteriaModel.constants),
      x = width.toDouble * Math.random(),
      y = height.toDouble * Math.random(),
      radians = Math.PI * 2 * Math.random(),
    )))

    initialNutrients.indices.foreach { i => initialNutrients.update(i, inputsAt(colony(i).x, colony(i).y).C) }

    def draw(): Unit = {
      drawNutrients()
      colony.foreach { bacteria =>
        drawBacteria(bacteria)
      }
    }

    def updateColony() = {
      if(tUntilFindWinner < maxT) {
        colony = colony.map(b => b.updated(inputsAt(b.x, b.y), delta, width, height))
        tUntilFindWinner = tUntilFindWinner + 1
      }
      else {
        def nutrientsDelta(bacteria: Bacteria, i: Int): Double = {
          val previousNutrients = initialNutrients(i)
          val currentnutrients = inputsAt(bacteria.x, bacteria.y).C
          previousNutrients - currentnutrients
        }

        val winners = colony.zipWithIndex
          .map { case (b, i) => (b, nutrientsDelta(b, i)) }
          .sortBy(_._2)
          .takeRight(winnerN)

        println(winners.last._1.constants)
        println(winners.map(_._2).sum)

        colony = new Array[Bacteria](n)
        winners.map(_._1).zipWithIndex.foreach { case (winner, i) =>
          (0 until cloneFactor).foreach { u =>
            colony.update(u + i * cloneFactor, winner.copy(
              x = width.toDouble * Math.random(),
              y = height.toDouble * Math.random(),
              radians = Math.PI * 2 * Math.random(),
              constants = mutated(winner.constants),
              accumulatedNutrients = 0,
              currentlyTumbling = false,
              timeUntilTumbleDone = 0,
            ))
          }
        }

        initialNutrients.indices.foreach { i => initialNutrients.update(i, inputsAt(colony(i).x, colony(i).y).C) }
        tUntilFindWinner = 0
      }
    }

    dom.window.setInterval(() => {
      clear()
      updateColony()
      draw()
    }, 10)

//    while(true) {
//      updateColony()
//    }

  }


}