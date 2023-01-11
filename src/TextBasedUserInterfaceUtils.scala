import javafx.scene.paint.{Color, PhongMaterial}

import scala.io.StdIn.readLine

//T6 class out of 2
object TextBasedUserInterfaceUtils {

  def showPromptToGetFile(): Unit = println("Please insert your file with the models. -- TYPE file_name.txt")

  def showPromptMinSize():Unit =
    println("Do you want to insert your Octree's smallest partition size? You can skip this if you want. -- TYPE INT")

  def showPromptMaxLevel(): Unit =
    println("Do you want to insert your Octree's depth limit? You can skip this if you want. -- TYPE INT")

  def showPromptColour(): Unit =
    println("What colour do you want your wired boxes to be?" +
    "\nBlue -- TYPE B /Red -- TYPE R /Green -- TYPE G")

  def getUserInput: String = readLine.trim.toUpperCase

  //getUserInputMaterial: handles the user answer returning a PhongMaterial depending on it
  def getUserInputMaterial: PhongMaterial = getUserInput match{
    case "B" | "b" => //BLUE
      val blueMaterial = new PhongMaterial()
      blueMaterial.setDiffuseColor(Color.rgb(0,0,150))
      blueMaterial
    case "G" | "g" => //GREEN
      val greenMaterial = new PhongMaterial()
      greenMaterial.setDiffuseColor(Color.rgb(0,255,0))
      greenMaterial
    case _ => //DEFAULT (RED)
      val redMaterial = new PhongMaterial()
      redMaterial.setDiffuseColor(Color.rgb(150,0,0))
      redMaterial
  }
  def printQuit(): Unit = {
    println("-- THIS IS YOUR OCTREE --")
  }

  def showPromptToApplyMethods(): Unit = println("Do you want to:" +
    "\nAdd shapes to Octree -- TYPE file_name.txt" +
    "\nScale your Octree to either 0.5 or 2 times its size? -- TYPE SC" +
    "\nApply effect sepia -- TYPE SE" +
    "\nRemove green -- TYPE G" +
    "\nQuit -- TYPE Q")

  def showPromptToScale(): Unit = println("What is the factor you want to apply? -- TYPE 0.5 or 2")

  def invalidOption(): Unit = println("Invalid option!")

}