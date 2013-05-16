package com.github.mdr.ascii.gui

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.coordAssign._
import com.github.mdr.ascii.layout._

object GUI extends App {

  SwingUtilities.invokeLater(new Runnable() {
    override def run {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
      Frame.pack()
      Frame.setVisible(true)
    }
  })

}

object Frame extends JFrame {

  setTitle("ASCII Graph Layout")
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setPreferredSize(new Dimension(1024, 640))
  val splitPane = new JSplitPane
  val inputTextPane = new JTextPane
  inputTextPane.setText("""|A,B,C
                           |A,C
                           |B,B
                           |D,E,F
                           |E,G""".stripMargin)
  val outputTextPane = new JTextPane
  inputTextPane.setFont(new Font(Font.MONOSPACED, inputTextPane.getFont.getStyle, inputTextPane.getFont.getSize))
  outputTextPane.setFont(new Font(Font.MONOSPACED, outputTextPane.getFont.getStyle, outputTextPane.getFont.getSize))
  outputTextPane.setEditable(false)
  inputTextPane.getDocument.addDocumentListener {
    new DocumentListener() {
      def insertUpdate(e: DocumentEvent) { refreshDiagram() }
      def removeUpdate(e: DocumentEvent) { refreshDiagram() }
      def changedUpdate(e: DocumentEvent) { refreshDiagram() }
    }
  }

  private def refreshDiagram() {
    try {
      val text = inputTextPane.getText
      val pieces = text.split("\n").toList.map(_.split(",").toList.map(_.replace("\\n", "\n")))
      val edges = pieces.flatMap { chunks ⇒ chunks.zip(chunks.tail) }
      val vertices = if (text.trim.isEmpty) Set[String]() else pieces.flatten.toSet
      val graph = Graph(vertices, edges)
      outputTextPane.setText(GraphLayout.renderGraph(graph, ToStringVertexRenderingStrategy,
        OptionsPanel.removeKinksBox.isSelected, OptionsPanel.compactifyBox.isSelected,
        OptionsPanel.unicodeBox.isSelected, OptionsPanel.verticalBox.isSelected))
    } catch {
      case e: Throwable ⇒
        outputTextPane.setText(e.getMessage)
        e.printStackTrace()
    }
  }

  splitPane.setTopComponent(new JScrollPane(inputTextPane))
  splitPane.setBottomComponent(new JScrollPane(outputTextPane))
  splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT)
  splitPane.setDividerLocation(0.5)
  splitPane.setResizeWeight(0.5)

  add(splitPane, BorderLayout.CENTER)
  add(OptionsPanel, BorderLayout.EAST)

  object OptionsPanel extends JPanel {

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    val removeKinksBox = new JCheckBox("Remove kinks")
    val compactifyBox = new JCheckBox("Compactify")
    val unicodeBox = new JCheckBox("Unicode")
    val verticalBox = new JCheckBox("Vertical")

    add(removeKinksBox)
    add(compactifyBox)
    add(unicodeBox)
    add(verticalBox)

    val refreshListener = new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        refreshDiagram()
      }
    }

    removeKinksBox.setSelected(true)
    removeKinksBox.addActionListener(refreshListener)
    compactifyBox.setSelected(true)
    compactifyBox.addActionListener(refreshListener)
    unicodeBox.setSelected(true)
    unicodeBox.addActionListener(refreshListener)
    verticalBox.setSelected(true)
    verticalBox.addActionListener(refreshListener)
  }

}