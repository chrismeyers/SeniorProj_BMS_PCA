﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Office.Tools.Ribbon;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using PCA_Addin.Forms;


namespace PCA_Addin
{
    public partial class PCA_Ribbon
    {
        // mesage box vars
        MessageBoxIcon error = MessageBoxIcon.Error;
        MessageBoxButtons ok = MessageBoxButtons.OK;

        private void Ribbon1_Load(object sender, RibbonUIEventArgs e)
        {

        }
        /// <summary>
        /// start button code
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void bt_Start_Click(object sender, RibbonControlEventArgs e)
        {

        }

        /// <summary>
        /// Purpose: reads in Data from current Quan Table and stores data in Global vars 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Read_bt_Click(object sender, RibbonControlEventArgs e)
        {
            cls_Read_FromXl read = new cls_Read_FromXl();
            try
            {
                read.Read_Section2();
            }
            catch
            {
                MessageBox.Show("error reading data make sure your sheet is named Quan Table and is in the correct Format -- set Columns -File-Group- up first then add the columns for your metabolites.", "Error", ok, error);
            }
        }

        /// <summary>
        /// Purpose: take the currently stored data in globals and write them back to the Excel sheet
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Write_bt_Click(object sender, RibbonControlEventArgs e)
        {
            try
            {
                cls_Read_FromXl read = new cls_Read_FromXl();              
                read.Write_Curent_Data2();
            }
            catch { MessageBox.Show("error writing data", "Error", ok, error); }
        }

        private void Calculate_bt_Click(object sender, RibbonControlEventArgs e)
        {
            
                cls_Data_Op DOP = new cls_Data_Op();
                DOP.Claculate_PCA();
                DOP.Write_Out_Calculation();
            
        }


        /// <summary>
        /// Upon selecting the scores plot option in the Excel add-in ribbon,
        /// form1 appears and prompts the user to specify the desired columns
        /// to be graphed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <author>Rowan Senior Project - Christian Marin, Chris Meyers, Derick Palos</author>
        private void Scores_bt_Click(object sender, RibbonControlEventArgs e)
        {
            Form1 scores = new Form1("Scores");
            scores.Show();
        }

        /// <summary>
        /// Upon selecting the loadings plot option in the Excel add-in ribbon,
        /// form1 appears and prompts the user to specify the desired columns
        /// to be graphed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <author>Rowan Senior Project - Christian Marin, Chris Meyers, Derick Palos</author>
        private void Loading_bt_Click(object sender, RibbonControlEventArgs e)
        {
            Form1 loadings = new Form1("Loadings");
            loadings.Show();
        }

        
       
        
    }
}
