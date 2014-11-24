using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.Office.Tools.Excel;
using System.IO;
using System.Configuration;
using System.Collections;

namespace PCA_Addin
{
   public class cls_Data_Op
    {

        int total_points = 0;// all points counted
        int total_files = 0;// all files being read 
        double[,] all_pointV_2D_array;// set the full length of our multidimentioanl array

        //varibales to hold our revived data 
        int Out_info = 0;
        double[] Out_s2;
        double[,] Out_v;

        /// <summary>
        /// Purpose: constructor for our class 
        /// </summary>
        /// 
        public cls_Data_Op()
        {
        }

        public void Claculate_PCA()
        {
            List<Array> all_point_values = new List<Array>();// all of the double values in our data
           
            //loop to fill all point values
            foreach (cls_PCA_File c in ThisAddIn.Go_QT.Lst_File)
            {
                //var to hold metabolite area values
                List<double> area_values = new List<double>();

                //loop to get area values
                foreach (cls_Metabolite_AreaNscale m in c.MetaboliteObjects)
                {
                    area_values.Add(m.AreaForIndividual);
                }

                //turn area values into an array 
                double[] areaValues_array = area_values.ToArray();
 
                //then add it to our array list
                all_point_values.Add(areaValues_array);
            }

            //set values for all the feilds we will pas to the calculation
            total_points = all_point_values[0].Length;
            total_files = ThisAddIn.Go_QT.Lst_File.Count;
            all_pointV_2D_array = new double[total_files, total_points];

            //set up an index and a loop that goes through all the values in our list array and then adds them into our 
            //2d array 
            int index = 0;
            foreach (Array a in all_point_values)
            {
                //set interior index and loop through each individual double 
                int interior_index = 0;
                foreach (double d in a)
                {
                        all_pointV_2D_array[index, interior_index] = d;
                        interior_index++;
                }
                index++;
            }

            //create a new instance of the PCA method 
            PCA_Method Perform_Calc = new PCA_Method();
            

            // perform the calculation and then send the values back to the holders we have for them at the top
            Perform_Calc.pcabuildbasis(all_pointV_2D_array, total_files, total_files, out Out_info, out Out_s2, out Out_v);

            // total_files  -- number of records/cases
            // total_points -- number of variables
            //Perform_Calc.pcabuildbasis(all_pointV_2D_array, total_files, total_points, out Out_info, out Out_s2, out Out_v);
            //double[,] vTranspose = Out_v;
            //Perform_Calc.copyandtranspose(Out_v, 0, nvars - 1, 0, nvars - 1, ref vTranspose, 0, nvars - 1, 0, nvars - 1);
        }


       /// <summary>
       /// Save results to excel
       /// </summary>
        public void Write_Out_Calculation()
        {
            //----------------------------------------------------first added sheet

            // create new excel sheet for the Ttest table
            Excel.Worksheet oSht = Globals.ThisAddIn.Application.Worksheets.Add();
            try
            {
                //find any duplicate sheets and remove them 
                for (int i = 1; i <= Globals.ThisAddIn.Application.Sheets.Count; i++)
                {
                    if (Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Name.Contains("PCA_Results"))
                    {
                        Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Delete();
                    }
                }

                //set the name of the new sheet
                oSht.Name = "PCA_Results";
            }
            catch
            {
                MessageBox.Show(" PCA_Results already exists please remove old before recalculating", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            int row = 1;
            // set the info
            // Excel.Range Info_Range = newWorksheet.get_Range("B2");
            if (Out_info == 1)
            {
                oSht.Cells[row, 1].value = "Info  " + Out_info + " PCA task solved!";
            }
            else
            {
                oSht.Cells[row, 1].value = "Info  " + Out_info + " PCA task failed!";
            }
            row =+2;


            // Variance Values
            //Excel.Range S2=newWorksheet.get_Range("B4");
            //S2.Value2 = "Variance (S2) Values:";
            
            oSht.Cells[row, 1].value = "Variance (S2) Values:";
            row++;
            oSht.Cells[row, 1].value = "PC #";
            oSht.Cells[row, 2].value = "Variance (s2)";
            //row incrementer 
            row++;
            // Excel.Range S2_Values = newWorksheet.get_Range("B" + row);
            foreach (double d in Out_s2)
            {
                // S2_Values = newWorksheet.get_Range("B"+row);
                // S2_Values.Value2 = d;
                oSht.Cells[row, 1].value = row-4;
                oSht.Cells[row, 2].value = d;
                row++;
            }

            //------------------------------------------------------------------------- second added sheet 

            Excel.Worksheet oSht2 = Globals.ThisAddIn.Application.Worksheets.Add();
            try
            {
                //find any duplicate sheets and remove them 
                for (int i = 1; i <= Globals.ThisAddIn.Application.Sheets.Count; i++)
                {
                    if (Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Name.Contains("Scores"))
                    {
                        Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Delete();
                    }
                }

                //set the name of the new sheet
                oSht2.Name = "Scores";
            }
            catch
            {
                MessageBox.Show("Scores already exists please remove old before recalculating", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            //Row 1 Group and Sample Text
            oSht2.Cells[1, 1].value = "Sample";
            oSht2.Cells[1, 2].value = "Group";
            //PC Headers
            for (int i = 3; i < Out_v.GetLength(0); i++)
            {
                oSht2.Cells[1, i].value = "PC" + (i - 2);
            }


            row = 2;

            foreach (cls_PCA_File f in ThisAddIn.Go_QT.Lst_File)
            {
                oSht2.Cells[row, 1].value = f.File_Name;
                oSht2.Cells[row, 2].value = f.Group_Name;
                row++;
            }

            row = 2;
            int col = 3;
            for (int i = 0; i < Out_v.GetLength(0); i++)
            {
               for (int j = 0; j < Out_v.GetLength(1); j++)
               {
                 oSht2.Cells[row,col].value = Out_v[i, j];
                 col++;
               }
               col = 3;    
               row++;
             }

        } // end void Write_Out_Calculation

    } // end class
} // end name space
