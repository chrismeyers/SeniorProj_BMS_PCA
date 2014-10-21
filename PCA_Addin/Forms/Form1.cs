using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

/// <summary>
/// Provides a form that allows the user to select desired columns to
/// be graphed.
/// </summary>
/// <author>Rowan Senior Project</author>
namespace PCA_Addin.Forms
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string col1 = textBox1.Text;
            string col2 = textBox2.Text;

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
