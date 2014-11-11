using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace PCA_Addin.Forms
{
    public partial class Form2 : Form
    {
        public Form2()
        {
            InitializeComponent();
            loadComboBoxes();
        }

        private void loadComboBoxes()
        {
            comboBox1.DataSource = PCA_graphing.getComboBoxData("Loadings");
           
        }

        //Submit
        private void button1_Click(object sender, EventArgs e)
        {

        }
        //Cancel
        private void button2_Click(object sender, EventArgs e)
        {

        }
    }
}
