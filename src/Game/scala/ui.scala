//package App // Zgodne z pakietem z Twojego main.scala
//
//import scalafx.application.JFXApp3
//import scalafx.scene.Scene
//import scalafx.scene.layout.{BorderPane, HBox, VBox}
//import scalafx.scene.control.{Button, Label}
//import scalafx.geometry.{Insets, Pos}
//
//object ScalperUI extends JFXApp3 {
//  override def start(): Unit = {
//    stage = new JFXApp3.PrimaryStage {
//      title = "Scalper - Texas Hold'em & Omaha"
//      scene = new Scene(800, 600) {
//
//        // Importujemy powszechnie używany standard do stylizacji
//        stylesheets.add("styles.css")
//
//        root = new BorderPane {
//          styleClass.add("poker-table")
//
//          // Górna sekcja: Karty wspólne i pula (Pot)
//          top = new VBox {
//            alignment = Pos.Center
//            spacing = 15
//            padding = Insets(30)
//            children = Seq(
//              new Label("Pula: 0") { styleClass.add("pot-label") },
//              new HBox {
//                alignment = Pos.Center
//                spacing = 10
//                children = Seq(
//                  new Label("[ ? ]") { styleClass.add("card") },
//                  new Label("[ ? ]") { styleClass.add("card") },
//                  new Label("[ ? ]") { styleClass.add("card") }
//                )
//              }
//            )
//          }
//
//          // Dolna sekcja: Przyciski akcji
//          bottom = new HBox {
//            alignment = Pos.Center
//            spacing = 20
//            padding = Insets(30)
//            children = Seq(
//              new Button("Fold") { styleClass.add("btn-fold") },
//              new Button("Check / Call") { styleClass.add("btn-call") },
//              new Button("Raise") { styleClass.add("btn-raise") }
//            )
//          }
//        }
//      }
//    }
//  }
//}