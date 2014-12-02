using System;
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
        String type = "";

        /// <summary>
        /// Form1 constructor.  Sets the text on the form to reflect the type of graph 
        /// selected.
        /// </summary>
        /// <param name="type">The type of graph (scores, loadings).</param>
        /// <author>Rowan Senior Project - Chris Meyers, Christian Marin, Derick Palos</author>
        public Form1(String type)
        {
            this.type = type;
            InitializeComponent();
            loadComboBoxes();

            label3.Text = "Select two columns for a " + type.ToLower() + " plot";
            Text = type + " Plot Column Select";
            
        }

        /// <summary>
        /// Populates the combo boxes with all princpal components.
        /// </summary>
        /// <author>Rowan Senior Project - Christian Marin</author>
        private void loadComboBoxes()
        {
            comboBox1.DataSource = PCA_graphing.getComboBoxData(type);
            comboBox2.DataSource = PCA_graphing.getComboBoxData(type);
        }

        /// <summary>
        /// Instantiates a PCA_graphing object and plots the specified graph with
        /// the selected principal components.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <author>Rowan Senior Project - Christian Marin, Derick Palos</author>
        private void button1_Click(object sender, EventArgs e)
        {
            string col1 = PCA_graphing.columnNumberToLetter(comboBox1.SelectedIndex + 3);
            string col2 = PCA_graphing.columnNumberToLetter(comboBox2.SelectedIndex + 3);

            PCA_graphing pcaGraphing = new PCA_graphing(type);
            this.Close(); //closes form

            pcaGraphing.plot(col1, col2);
        }

        /// <summary>
        /// Closes the current input form.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <author>Rowan Senior Project</author>
        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }

    }
}
