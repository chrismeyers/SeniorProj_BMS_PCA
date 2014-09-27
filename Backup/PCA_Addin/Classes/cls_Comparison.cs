
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCA_Addin
{
    /// <summary>
    /// Purpose: to set up our comparison tables. Each comparison table contains list of comparisons
    /// </summary>
    public class cls_Comparison
    {
        //internal string vars 
        internal string Numerator1;             
        internal string Denominator2;  

        //public list of cls_Results objects 
        public List<cls_Results> Results_List;

        //properties for the Numerator string 
        public string Numerator_1
        {
            get{ return Numerator1; }
            set{ Numerator1 = value; }
        }

        //properties for the Denominator string 
        public string Denominator_2
        {
            get
            {
                return Denominator2;
            }
            set
            {
                Denominator2 = value;
            }
        }

        /// <summary>
        /// Purpose: Constructor for class object 
        /// </summary>
        public cls_Comparison()
        {
            //intaniates our string's and object list
           Results_List = new List<cls_Results>();
           Numerator1 = String.Empty;
           Denominator2 = String.Empty;
        }

        /// <summary>
        /// Purpose: Overload-secondary constructor passing two string paramiters 
        /// </summary>
        /// <param name="N"></param>
        /// <param name="D"></param>
        public cls_Comparison(string N, string D)
        {
            //instatiates our list 
            Results_List = new List<cls_Results>();

            //sets the two strings equal to the passed paramiters
            Numerator1 = N;
            Denominator2 = D;
        }
        
    }// end of class
}//end of namespace
