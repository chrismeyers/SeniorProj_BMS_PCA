using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using PCA_Addin.Classes;

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
            pcaGraphing.Scores_Plot(col1, col2);
            this.Close();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
