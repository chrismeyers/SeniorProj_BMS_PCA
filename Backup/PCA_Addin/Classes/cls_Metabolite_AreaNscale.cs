
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCA_Addin
{
    /// <summary>
    /// Purpose: create a meabolite area and scale class to hold info sutch as the individual area for a single metabolite and the scaled area of that metabolite
    /// </summary>  
    public class cls_Metabolite_AreaNscale
    {

        double Area_ForIndividual; // area value for the indvidual metabolite
        double Area_Normalized;    // normalized area value
        double Area_Scaled;        // scaled area value


        // Properties
        public double AreaForIndividual            // property so that the value can be accesed from the outside 
        {
            get{ return Area_ForIndividual; }
            set{ Area_ForIndividual = value; }
        }

        public double AreaNormalized               // property so that the value can be accesed from the outside 
        {
            get{ return Area_Normalized; }
            set{ Area_Normalized = value; }
        }

        public double AreaScaled                   // property so that the value can be accesed from the outside 
        {
            get { return Area_Scaled; }
            set { Area_Scaled = value; }
        }

        /// <summary>
        /// Purpose: a constructor whitch passes in a new double value every time the object of this class is created 
        /// Import: double NewArea
        /// </summary>
        /// <param name="NewArea"></param>
        public cls_Metabolite_AreaNscale(double NewArea)
        {
            Area_ForIndividual = 0.0;
            Area_Normalized = 0.0;
            Area_Scaled = 0.0;
            Area_ForIndividual = NewArea;
        }

        /// <summary>
        /// Purpose: Overload -a constructor that passes no arguments 
        /// </summary>
        public cls_Metabolite_AreaNscale()
        {
        }

    } // end of class
} // end of namespace 
