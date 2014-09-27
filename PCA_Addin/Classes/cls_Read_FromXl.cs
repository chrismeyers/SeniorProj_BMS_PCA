// Excel Addin for t-Test analysis of the metabolomics data
// Created : 02/15/2014
// Modified: 05/08/2014
// (c) 2014 AIM (Mathew Cintron & Serhiy Hnastyhyn)
//
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
    /// <summary>
    /// Purpose: class to read the data already in the excel and send it directly into the Global Lists 
    /// </summary>
    class cls_Read_FromXl
    {
        //set up initial excel vars 
        Excel.Worksheet Worksheet2;
        Excel.Workbook activeWorkbook;
        Excel.Range excelRange2;


        //set up a blank counter 
        int Blanks;

        //a list of double arrays containing every individual metabolite decimal number
        public List<Array> Metabolite_Areas_Lists;
       

        /// <summary>
        /// Purpose: Constructor for the class Instantiates our inital vars 
        /// </summary>
       public cls_Read_FromXl()
        {
           //instantiate vars 
            try
            {
                Blanks = 0; 
                Worksheet2 = Globals.ThisAddIn.Application.Sheets[0]; 
                activeWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;
                Metabolite_Areas_Lists = new List<Array>();     
            }
            catch { }
        }

        
        /// <summary>
        /// Purpose: reads in the data in excel into Global lists
        /// </summary> 
        public void Read_Section2()
        {
           
         
            //clears the current data 
            ThisAddIn.Go_QT = new cls_QT();

            // finds the Quan Table sheet 
            for (int i = 1; i <= Globals.ThisAddIn.Application.Sheets.Count; i++)
            {
                if (Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Name.Contains("Quan Table Transformations"))
                {
                    Worksheet2 = Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i];
                    break;
                }
                else if (Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Name.Contains("Quan Table"))
                {
                    Worksheet2 = Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i];
                    break;
                }
            }

        

            // row incriments reset with each run of the program
            int rowincrementer2 = 1;
        
            // set up our inital vars 
            string curentText2 = string.Empty;
            cls_PCA_File myRow;
            cls_Metabolite_AreaNscale myCell;

            // starting index of excel cell and starting range
            Int32 iRow = 2; 
            excelRange2 = (Excel.Range)Worksheet2.Cells[iRow, 1];

        

            //loop through values of excel sheet cells while values are not blank
            while (!string.IsNullOrWhiteSpace(excelRange2.Text.ToString()))
            {
                //reset values based on incrimenters
                excelRange2 = (Excel.Range)Worksheet2.Cells[1, rowincrementer2];
                ThisAddIn.Go_QT.headers_for_QT.Add(excelRange2.Value2);

                try
                {// check for incorect headers
                    if (!excelRange2.Value2.Contains("Group") &&
                        !excelRange2.Value2.Contains("File") && 
                        !excelRange2.Value2.Contains("Title"))
                    {
                        //add metabolites in objects to our metabolite list 
                        cls_Metabolite_Files myRowz = new cls_Metabolite_Files();
                        myRowz.M_name = excelRange2.Value2;
                        ThisAddIn.Go_QT.Lst_Metabolite.Add(myRowz);
                    }

                    //increment the row value
                    rowincrementer2++;              
                }
                catch { }
            }
            //rest values for the next loop 
            excelRange2 = (Excel.Range)Worksheet2.Cells[iRow, 1];
            rowincrementer2 = 1;

            //while loop desined to read from excel until it hits a blank cell for the next section of data
            while (!string.IsNullOrWhiteSpace(excelRange2.Text.ToString()))
            {
                //set new cls_NMR_File object and its file name paramiter
                myRow = new cls_PCA_File();
                myRow.File_Name = excelRange2.Value2;


                //ajust range value to get the group name value
                excelRange2 = (Excel.Range)Worksheet2.Cells[iRow, 2];
                myRow.Group_Name = excelRange2.Value2;

              // interior for loop to set the values for each of the decimals 
                for (int jCol = 3; jCol - 3 <= ThisAddIn.Go_QT.Lst_Metabolite.Count; jCol++)
                {
                    excelRange2 = (Excel.Range)Worksheet2.Cells[iRow, jCol];
                    try
                    {
                        if (string.IsNullOrWhiteSpace(excelRange2.Text.ToString()) && jCol - 3 != ThisAddIn.Go_QT.Lst_Metabolite.Count)
                        {
                            excelRange2.Interior.Color = System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Red);
                            excelRange2.Value2 = 0;
                            Blanks++;
                        }        

                        myCell = new cls_Metabolite_AreaNscale(Convert.ToDouble(excelRange2.Value2 + ""));

                        myRow.MetaboliteObjects.Add(myCell);
                    }
                    catch (Exception c)
                    {}
                    // mark blanks with red 
                         
                }

                //add the new object to the global list for NMR file 
                ThisAddIn.Go_QT.Lst_File.Add(myRow);

                iRow++;
                excelRange2 = (Excel.Range)Worksheet2.Cells[iRow, 1];
                rowincrementer2++;
            };

            // report the process  status : output to status bar
            if (Blanks == 0)
            {
               MessageBox.Show( "data read complete ready to group or write back","Read",MessageBoxButtons.OK,MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show( "Data read complete! \n \n Addin found "+Blanks+" Blanks ? \n \n Blanks may cause errors. \n \n Adivise remove blanks and Re Read.");
            }
            
        }

      
        /// <summary>
        /// Purpose: a method to write out the second section of the data
        /// </summary>
        public void Write_Curent_Data2()
        {
            Excel.Worksheet newWorksheet = new Excel.Worksheet();

            bool Stop_QT_Termination = false;
            // find the sheet labeled Quan Table and remove it 
            for (int i = 1; i <= Globals.ThisAddIn.Application.Sheets.Count; i++)
            {
                if (Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Name.Contains("Quan Table"))
                {
                    try
                    {
                        Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[i].Delete();
                    }
                    catch (Exception e) { Stop_QT_Termination = true; }
                }
            }

            // addthe new Quan Table sheet 
            if (Stop_QT_Termination == false)
            {
                newWorksheet = Globals.ThisAddIn.Application.ActiveWorkbook.Sheets.Add();    // use to set the data on a different sheet 
                try
                {
                    newWorksheet.Name = "Quan Table"; // changes the name of the targeted sheet
                }
                catch { }
            }
            else
            {
                try
                {
                    newWorksheet = Globals.ThisAddIn.Application.ActiveWorkbook.Sheets[1];
                }
                catch (Exception x) { MessageBox.Show("" + x); }
            }


            int rowincrementer2 = 2;// the row incriments to be reset with each run of the program
            int indexIncrementer = 0;// increment the index of every pice of our data allowing us to gradualy see the whole thing 
            int header_incrementer = 1;// increment the value of the header 

            //display the headers for the entire quan tabel 
            foreach (string s in ThisAddIn.Go_QT.headers_for_QT)
            {
                Excel.Range header_for_QT = (Excel.Range)newWorksheet.Cells[1, header_incrementer];
                header_for_QT.Value2 = s;
                header_incrementer++;
            }


            //place the data from our globals into excel so that our users can view the parced info in mass if they so choose 
            foreach (cls_PCA_File o in ThisAddIn.Go_QT.Lst_File)
            {
                //set rage for File Name
                Excel.Range SScolomn1 = newWorksheet.get_Range("A" + rowincrementer2);
                SScolomn1.Value2 = ThisAddIn.Go_QT.Lst_File[indexIncrementer].File_Name;

                //  offset and set each indvidual metabolite to its own column
                Excel.Range SScolomn3 = newWorksheet.get_Range("B" + rowincrementer2);
                SScolomn3.Value2 = ThisAddIn.Go_QT.Lst_File[indexIncrementer].Group_Name;

                
                    Excel.Range SScolomn4 = newWorksheet.get_Range("B" + rowincrementer2);

                    int indexer = 0;
                    foreach (object d in ThisAddIn.Go_QT.Lst_File[indexIncrementer].MetaboliteObjects)
                    {
                        try
                        {

                            SScolomn4 = SScolomn4.get_Offset(0, +1);
                            SScolomn4.Value2 = o.MetaboliteObjects[indexer].AreaForIndividual;

                            indexer++;
                        }
                        catch (Exception w) { MessageBox.Show("error" + w,"Error",MessageBoxButtons.OK,MessageBoxIcon.Error); }

                    }
                


                // use row incrementer to systematicly have data grow row by row
                rowincrementer2++;
                indexIncrementer++;
            }

          

        }

       
    }//end of class
}// end of namespace
