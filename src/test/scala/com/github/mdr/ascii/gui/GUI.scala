package com.github.mdr.ascii.gui

import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.FlowLayout
import java.awt.Font

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.GraphLayout
import com.github.mdr.ascii.layout.ToStringVertexRenderingStrategy

import javax.swing.JCheckBox
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTextPane
import javax.swing.SwingUtilities
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

object GUI extends App {

  SwingUtilities.invokeLater(new Runnable() {
    override def run {
      Frame.pack()
      Frame.setVisible(true)
    }
  })

}

object Frame extends JFrame {

  setTitle("ASCII Graph Layout")
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setPreferredSize(new Dimension(640, 480))
  val splitPane = new JSplitPane
  val inputTextPane = new JTextPane
  inputTextPane.setText("""A,B
B,C
A,C""")
  val outputTextPane = new JTextPane
  inputTextPane.setFont(new Font(Font.MONOSPACED, outputTextPane.getFont.getStyle, outputTextPane.getFont.getSize))
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
      val pieces = text.split("\n").toList.map(_.split(",").toList)
      val edges = pieces.collect { case List(a, b) ⇒ a -> b }
      val vertices = pieces.flatten.toSet
      val graph = Graph(vertices, edges)
      outputTextPane.setText(GraphLayout.renderGraph(graph, ToStringVertexRenderingStrategy,
        OptionsPanel.removeKinksBox.isSelected, OptionsPanel.compactifyBox.isSelected))
    } catch {
      case e: Throwable ⇒ outputTextPane.setText(e.getMessage)
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
    setLayout(new FlowLayout)
    val removeKinksBox = new JCheckBox("Remove kinks")
    val compactifyBox = new JCheckBox("Compactify")
    add(removeKinksBox)
    add(compactifyBox)
    removeKinksBox.setSelected(true)
    removeKinksBox.addChangeListener(new ChangeListener() {
      def stateChanged(e: ChangeEvent) {
        refreshDiagram()
      }
    })
    compactifyBox.setSelected(true)
    compactifyBox.addChangeListener(new ChangeListener() {
      def stateChanged(e: ChangeEvent) {
        refreshDiagram()
      }
    })
  }

}