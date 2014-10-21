using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Data;
using System.Xml.Linq;
using System.Reflection;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using System.Drawing;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.VisualStudio.Tools.Applications.Runtime;
using PCA_Addin.Forms;

/// <summary>
/// The purpose of this class it to provide visualization functionality
/// for the PCA Excel add-in.  Two plots will be available: scores plot
/// and loadings plot.
/// </summary>
/// <author>Rowan Senior Project</author>
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
        public void scoresPlot(string col1, string col2){
            //MessageBox.Show("Column " + col1 + " and " + "column " + col2 + " selected.");
            String pc1 = col1.ToUpper();
            String pc2 = col2.ToUpper();

            Excel.Worksheet ws;
            Excel.Workbook activeWorkbook;
            Excel.Range chartRange;
            object misValue = System.Reflection.Missing.Value;

            //Get current spreadsheet
            ws = Globals.ThisAddIn.Application.Sheets[1];
            activeWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;

            //Define chart specs
            Excel.ChartObjects xlCharts = (Excel.ChartObjects)ws.ChartObjects(Type.Missing);
            Excel.ChartObject myChart = (Excel.ChartObject)xlCharts.Add(10, 80, 300, 250);
            Excel.Chart chartPage = myChart.Chart;

            //Define data strucures needed for data manipulation and plotting.
            int numRows = ws.UsedRange.Rows.Count;
            ArrayList groups = new ArrayList();
            ArrayList groupPoints = new ArrayList();
            Dictionary<String, ArrayList> plotData = new Dictionary<String, ArrayList>();
             
            if (errorCheck(pc1, pc2)){
                //Asks for new valid columns upon error.
                newColumns();
            }
            else{
                //Inititally make previous group first group name.
                //This will be used to check if the group changed.
                //If it did, add the previous data collected to a dictionary
                //with Key=previous group and Value=Arraylist of the data 
                //corresponding to the key.
                String prevGrp = ws.Cells[2, 2].Value.ToString();

                for (int i = 2; i < numRows+1; i++) {
                    String currentGrp = ws.Cells[i, 2].Value.ToString();

                    //Add the data for the previous group to the dictionary
                    if (!currentGrp.Equals(prevGrp)){
                        groups.Add(prevGrp);
                        //ArrayList needs to be cloned or data from next group will overwrite
                        plotData.Add(prevGrp, (ArrayList)groupPoints.Clone());
                        groupPoints.Clear();
                    }
                    

                    //Get current row x and y values.
                    //Add them to an array, add this array to the group ArrayList
                    Double[] currentPoints = new Double[2];
                    String currentX = ws.Cells[i, columnLetterToNumber(pc1)].Value.ToString();
                    String currentY = ws.Cells[i, columnLetterToNumber(pc2)].Value.ToString();
                    //MessageBox.Show(currentGrp + ": " + currentX + ", " + currentY);
                    currentPoints[0] = Convert.ToDouble(currentX);
                    currentPoints[1] = Convert.ToDouble(currentY);
                    //MessageBox.Show(currentGrp + ": " + currentPoints[0] + ", " + currentPoints[1]);
                    groupPoints.Add(currentPoints);

                    //Add the data for the last row
                    if (i == numRows){
                        groups.Add(currentGrp);
                        plotData.Add(currentGrp, (ArrayList)groupPoints.Clone());
                        groupPoints.Clear();
                    }

                    prevGrp = currentGrp;
                }

                /*
                chartRange = ws.get_Range("B:B" + "," + pc1 + ":" + pc1 + "," + pc2 + ":" + pc2);
                chartPage.SetSourceData(chartRange, misValue);
                 */
                chartPage.ChartType = Excel.XlChartType.xlXYScatter;
                 
                //Loop through each group and make a series for each one.
                foreach (String group in groups) {
                    //MessageBox.Show(group);
                    Excel.SeriesCollection seriesCollection = (Excel.SeriesCollection)myChart.Chart.SeriesCollection();
                    Excel.Series currentGroup = seriesCollection.NewSeries();
                    currentGroup.Name = group;

                    //Get point values from associated group.
                    ArrayList values = new ArrayList();
                    values = plotData[group];

                    Double[] x = new Double[values.Count];
                    Double[] y = new Double[values.Count];

                    //Loop through all points in group and save to x and y array.
                    for (int val = 0; val < values.Count; val++) {
                        Double[] points = (Double[])values[val];
                        //MessageBox.Show(points[0] + ", " + points[1]);

                        x[val] = points[0];
                        y[val] = points[1];
                        
                    }
                    currentGroup.XValues = x; //X values (PC1)
                    currentGroup.Values = y;  //Y values (PC2)

                }
                
            }
            //MessageBox.Show("Value is: " + (ws.Cells[10,10] as Excel.Range).Value);
        }

        /// <summary>
        /// Allows user to enter new point upon an error.
        /// </summary>
        /// <author>Rowan Senior Project</author>
        private void newColumns() {
            Form1 scoresError = new Form1();
            scoresError.Show();
        }

        /// <summary>
        /// Converts column letter to its associated numerical value.
        /// Ex: A->1, B->2, etc.
        /// </summary>
        /// <author>Rowan Senior Project</author>
        private int columnLetterToNumber(String col) {
            int conversion = 0;
            string currentCol = col;
            for (int iChar = currentCol.Length - 1; iChar >= 0; iChar--)
            {
                char colPiece = currentCol[iChar];
                int colNum = colPiece - 64;
                conversion = conversion + colNum * (int)Math.Pow(26, col.Length - (iChar + 1));
            }
            return conversion;
        }

        /// <summary>
        /// Checks for column input errors.
        /// </summary>
        /// <author>Rowan Senior Project</author>
        private Boolean errorCheck(String pc1, String pc2) {
            Boolean error = false;
            //Check for unique columns
            if (pc1.Equals(pc2)){
                MessageBox.Show("Please select two different columns.", "Error");
                error = true;
            }
            //Ensure selected columns are not Sample or Group (A or B).
            if (pc1.Equals("A") || pc1.Equals("B") || pc2.Equals("A") || pc2.Equals("B")){
                MessageBox.Show("Columns A and B are reserved.  Please choose another column.", "Error");
                error = true;
            }
            //TODO: only allow valid columns (Letters)
            return error;
        }
    }

}
