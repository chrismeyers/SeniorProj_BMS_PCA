
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCA_Addin
{
    public class cls_Group_Files_Data
    {
        // string of group name 
        public String Group_Name;

        //list of Grouped files 
        public List<cls_PCA_File> GroupContents;

        /// <summary>
        /// prorerty for seting the group name
        /// </summary>
        public String GroupName
       {
           get
           {
               return Group_Name;
           }
           set
           {
               Group_Name = value;
           }
       }

        /// <summary>
        /// Purpose: instantiate the object of the class and use the group name to find the apropriate objects to add to its contents
        /// Import: string GroupName
        /// Export: new cls_NMR_File object
        /// </summary>
        /// <param name="newGroupName"></param>
       public cls_Group_Files_Data(string newGroupName)
       {
           GroupContents = new List<cls_PCA_File>();
           Group_Name = newGroupName;

          // checks the object_files list for matchs of group name and then adds them to the Contents list inside this class 
           // and finally sends the fully pakaged group into the global group list 
           foreach (cls_PCA_File o in ThisAddIn.Go_QT.Lst_File)
           {
               if(o.Group_Name == Group_Name)
               {
                   GroupContents.Add(o);
               }
           }
           ThisAddIn.Go_QT.Lst_Group.Add(this);
         }
      }// end of class
  
}// end of namespace 
