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

/// <summary>
/// The purpose of this class it to provide visualization functionality
/// for the PCA Excel add-in.  Two plots will be available: scores plot
/// and loadings plot.
/// </summary>
namespace PCA_Addin
{
    public class PCA_graphing
    {

        /// <summary>
        /// A scores plot is a statistical analysis that identifies unusual
        /// clusters from a data set and plots them based on their associated
        /// principal component scores.
        /// </summary>
        /// <param name="col1">The first column to be used in plot (PC1)</param>
        /// <param name="col2">The seconf column to be used in plot (PC2)</param>
        /// <author>Rowan Senior Project</author>
        public void scoresPlot(string col1, string col2)
        {
            //TODO: Check for same columns
            //MessageBox.Show("Column " + col1 + " and " + "column " + col2 + " selected.");

            Excel.Worksheet ws;
            Excel.Workbook activeWorkbook;
            Excel.Range chartRange;
            object misValue = System.Reflection.Missing.Value;

            ws = Globals.ThisAddIn.Application.Sheets[1];
            activeWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;

            Excel.ChartObjects xlCharts = (Excel.ChartObjects)ws.ChartObjects(Type.Missing);
            Excel.ChartObject myChart = (Excel.ChartObject)xlCharts.Add(10, 80, 300, 250);
            Excel.Chart chartPage = myChart.Chart;

            chartRange = ws.get_Range("B:B," + col1 + ":" + col1 + "," + col2 + ":" + col2);
            chartPage.SetSourceData(chartRange, misValue);
            chartPage.ChartType = Excel.XlChartType.xlXYScatter;

            //MessageBox.Show("Value is: " + (ws.Cells[10,10] as Excel.Range).Value);
        }
    }

}
