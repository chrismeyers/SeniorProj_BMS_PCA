namespace PCA_Addin
{
    partial class PCA_Ribbon : Microsoft.Office.Tools.Ribbon.RibbonBase
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        public PCA_Ribbon()
            : base(Globals.Factory.GetRibbonFactory())
        {
            InitializeComponent();
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.tab1 = this.Factory.CreateRibbonTab();
            this.PCA_data = this.Factory.CreateRibbonGroup();
            this.Read_bt = this.Factory.CreateRibbonButton();
            this.Write_bt = this.Factory.CreateRibbonButton();
            this.Calculate_bt = this.Factory.CreateRibbonButton();
            this.Scores_bt = this.Factory.CreateRibbonButton();
            this.Loading_bt = this.Factory.CreateRibbonButton();
            this.tab1.SuspendLayout();
            this.PCA_data.SuspendLayout();
            // 
            // tab1
            // 
            this.tab1.ControlId.ControlIdType = Microsoft.Office.Tools.Ribbon.RibbonControlIdType.Office;
            this.tab1.Groups.Add(this.PCA_data);
            this.tab1.Label = "TabAddIns";
            this.tab1.Name = "tab1";
            // 
            // PCA_data
            // 
            this.PCA_data.Items.Add(this.Read_bt);
            this.PCA_data.Items.Add(this.Write_bt);
            this.PCA_data.Items.Add(this.Calculate_bt);
            this.PCA_data.Items.Add(this.Scores_bt);
            this.PCA_data.Items.Add(this.Loading_bt);
            this.PCA_data.Label = "PCA Data Functions";
            this.PCA_data.Name = "PCA_data";
            // 
            // Read_bt
            // 
            this.Read_bt.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.Read_bt.Image = global::PCA_Addin.Properties.Resources.documents;
            this.Read_bt.Label = "Read";
            this.Read_bt.Name = "Read_bt";
            this.Read_bt.ShowImage = true;
            this.Read_bt.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.Read_bt_Click);
            // 
            // Write_bt
            // 
            this.Write_bt.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.Write_bt.Image = global::PCA_Addin.Properties.Resources.copy;
            this.Write_bt.Label = "Write";
            this.Write_bt.Name = "Write_bt";
            this.Write_bt.ShowImage = true;
            this.Write_bt.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.Write_bt_Click);
            // 
            // Calculate_bt
            // 
            this.Calculate_bt.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.Calculate_bt.Image = global::PCA_Addin.Properties.Resources.applications;
            this.Calculate_bt.Label = "Calculate";
            this.Calculate_bt.Name = "Calculate_bt";
            this.Calculate_bt.ShowImage = true;
            this.Calculate_bt.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.Calculate_bt_Click);
            // 
            // Scores_bt
            // 
            this.Scores_bt.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.Scores_bt.Image = global::PCA_Addin.Properties.Resources.scores1;
            this.Scores_bt.Label = "Scores Plot ";
            this.Scores_bt.Name = "Scores_bt";
            this.Scores_bt.ShowImage = true;
            this.Scores_bt.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.Scores_bt_Click);
            // 
            // Loading_bt
            // 
            this.Loading_bt.ControlSize = Microsoft.Office.Core.RibbonControlSize.RibbonControlSizeLarge;
            this.Loading_bt.Image = global::PCA_Addin.Properties.Resources.scores;
            this.Loading_bt.Label = "Loading Plot";
            this.Loading_bt.Name = "Loading_bt";
            this.Loading_bt.ShowImage = true;
            this.Loading_bt.Click += new Microsoft.Office.Tools.Ribbon.RibbonControlEventHandler(this.Loading_bt_Click);
            // 
            // PCA_Ribbon
            // 
            this.Name = "PCA_Ribbon";
            this.RibbonType = "Microsoft.Excel.Workbook";
            this.Tabs.Add(this.tab1);
            this.Load += new Microsoft.Office.Tools.Ribbon.RibbonUIEventHandler(this.Ribbon1_Load);
            this.tab1.ResumeLayout(false);
            this.tab1.PerformLayout();
            this.PCA_data.ResumeLayout(false);
            this.PCA_data.PerformLayout();

        }

        #endregion

        internal Microsoft.Office.Tools.Ribbon.RibbonTab tab1;
        internal Microsoft.Office.Tools.Ribbon.RibbonGroup PCA_data;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton Read_bt;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton Write_bt;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton Calculate_bt;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton Scores_bt;
        internal Microsoft.Office.Tools.Ribbon.RibbonButton Loading_bt;
    }

    partial class ThisRibbonCollection
    {
        internal PCA_Ribbon Ribbon1
        {
            get { return this.GetRibbon<PCA_Ribbon>(); }
        }
    }
}
