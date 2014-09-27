
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCA_Addin
{
    /// <summary>
    /// Purpose: Stores results of tTest calculataions
    /// </summary>
    public class cls_Results
    {

        public int info;
        public double[] s2;
        public double[,] v;

        /// <summary>
        /// Purpose: constructor without arguments
        /// </summary>
       public cls_Results()
       {
       }

        /// <summary>
        /// Purpose: constructor to pass all values into their apropriate place 
        /// Import: 6 double values 
        /// </summary>
       public cls_Results( int i, double[] s, double[,] v2)
       {
           info = i;
           s2 = s;
           v = v2;
       }

    } // end class
} // end name space
