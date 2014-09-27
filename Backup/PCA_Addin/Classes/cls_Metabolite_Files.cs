
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCA_Addin
{
    /// <summary>
    /// Purpose:holds first portion of "results.txt" - metabolites list
    /// </summary>
    public class cls_Metabolite_Files
    {
        //internal string and doubles holding the name the left shift and the right shift 
        internal string MName;
        internal double LeftShift;
        internal double RightShift;

        // sets lists used to place data into excel 
        public  List<object> MName_List;
        public  List<object> LeftShist_List;
        public List<object> RightShift_List;

        // property values 
        public string M_name
        {
            get
            {
                return MName;
            }
            set
            {
                MName = value;
            }
        }
        public double Left_Shift
        {
            get
            {
                return LeftShift;
            }
            set
            {
                LeftShift = value;
            }
        }  
        public double Right_shift
        {
            get
            {
                return RightShift;
            }
            set
            {
                RightShift = value;
            }
        }  

        /// <summary>
        /// Purpose: constructor initilizes the inital values 
        /// </summary>
        public cls_Metabolite_Files()
        {
            // individual values
            this.MName = string.Empty;
            this.LeftShift = 0.0;
            this.RightShift = 0.0;

            //list values
            MName_List = new List<object>();
            LeftShist_List = new List<object>();
            RightShift_List = new List<object>();

        }
     
        /// <summary>
        ///  Purpose: method that sends values to variables for each piece of data in the first section 
        ///  Import: String Array
        /// </summary>
        /// <param name="values"></param>
        public void MetaboliteRecords(string[] values)
        {

            // individual values
            this.LeftShift = Convert.ToDouble(values[0]);
            this.RightShift = Convert.ToDouble(values[1]);
            this.MName = values[2];

            //holds our data that we place out to excel
            try
            {
                MName_List.Add(values[2]);
                LeftShist_List.Add(values[0]);
                RightShift_List.Add(values[1]);
                
            }
            catch 
            {
            }
        }
    } // end of class
}//end of namespace
