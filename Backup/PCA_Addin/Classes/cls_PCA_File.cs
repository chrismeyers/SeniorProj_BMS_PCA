
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
    /// Purpose: this class is where object by object all data from the second section lines are held
    /// </summary>
    public class cls_PCA_File
    {
        // list of Metabilite_Areas objects
        public List<cls_Metabolite_AreaNscale> MetaboliteObjects;

        //internal vars 
        internal String GroupName ;
        internal String FileName;
        internal String TitleName;
        internal Double Sum;

        //Declared propertys
        public String File_Name
        {
            get
            {
                return FileName;
            }
            set
            {
                FileName = value;
            }

        }
        public String Title_Name
        {
            get
            {
                return TitleName;
            }
            set
            {
                TitleName = value;
            }
        }
        public String Group_Name
        {
            get
            {
                return GroupName;
            }
            set
            {
                GroupName = value;
            }
        }
        public Double Sum_Value
        {
            get
            {
                return Sum;
            }
            set
            {
                Sum = value;
            }

        }


        /// <summary>
        /// Purpose: constructor whitch builds our lists for the individual metabolite area 
        /// </summary>
        public cls_PCA_File()
        {
            MetaboliteObjects = new List<cls_Metabolite_AreaNscale>();
        }

    }// end of class
}// end of namespace
