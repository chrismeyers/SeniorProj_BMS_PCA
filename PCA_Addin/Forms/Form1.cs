﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using System.Diagnostics;


/// <summary>
/// Provides a form that allows the user to select desired columns to
/// be graphed.
/// </summary>
/// <author>Rowan Senior Project - Christian Marin, Chris Meyers, Derick Palos</author>
namespace PCA_Addin.Forms
{
     
    public partial class Form1 : Form
    {
       
        public Form1()
        {
            InitializeComponent();
            loadComboBoxes();
        }

        private void loadComboBoxes()
        {
            comboBox1.DataSource = PCA_graphing.getComboBoxData("Scores");
            comboBox2.DataSource = PCA_graphing.getComboBoxData("Scores");
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string col1 = PCA_graphing.columnNumberToLetter(comboBox1.SelectedIndex + 3);
            string col2 = PCA_graphing.columnNumberToLetter(comboBox2.SelectedIndex + 3);

            PCA_graphing pcaGraphing = new PCA_graphing();
            this.Close(); //closes form
            pcaGraphing.scoresPlot(col1, col2);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }

    }
}
