using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.VisualStudio.Tools.Applications.Runtime;
using System.Data;
using System.Xml.Linq;
using System.Reflection;
using System.Windows.Forms;
using System.Drawing;


/*
 * Senior Project
 * 
 * The purpose of this class it to provide visualization functionality
 * for the PCA Excel add-in.  Two plots will be available: scores plot
 * and loadings plot.
 * 
 */
namespace PCA_Addin
{
    public class PCA_graphing
    {
        /* 
         * Scores plot is a statistical analysis that 
         * 
         * @param col1 The first column to be used in plot (PC1)
         * @param col2 The seconf column to be used in plot (PC2)
         * @author Senior Project
         */
        public void scoresPlot(string col1, string col2)
        {
            //TODO: Check for same columns
            MessageBox.Show("Column " + col1 + " and " + "column " + col2 + " selected.");

            Excel.Worksheet ws;
            Excel.Workbook activeWorkbook;

            ws = Globals.ThisAddIn.Application.Sheets[1];
            activeWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;
            /*
            Microsoft.Office.Tools.Excel.Chart chart1 = this.Controls.AddChart(this.Range["D2", "H12"], "chart1");

            chart1.SetSourceData(this.Range["A1", "B5"], Excel.XlRowCol.xlColumns);
            chart1.ChartType = Excel.XlChartType.xlXYScatterLines;

            Excel.ChartGroup group = (Excel.ChartGroup)chart1.XYGroups(1);

            MessageBox.Show("The axis group is: " +group.AxisGroup.ToString());
             */

            MessageBox.Show("Value is: " + (ws.Cells[10,10] as Excel.Range).Value);
        }
    }

}
