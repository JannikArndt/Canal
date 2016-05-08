using Canal.Properties;
using Logging;
using Model;
using System;
using System.ComponentModel;
using System.Linq;
using System.Windows.Forms;

namespace Canal.Utils
{
    public class Analyzer
    {
        public void AnalyzeFile(object sender, DoWorkEventArgs doWorkEventArgs)
        {
            var cobolFile = (CobolFile)doWorkEventArgs.Argument;

            // 1. Look for Identification, Environment, Data and Procedure Divion
            cobolFile.DivisionsAndSection = TextUtil.Instance.FindDivisions(cobolFile.Text);

            // 2. Analyze and cache files in folder and folders on same level
            FileUtil.Instance.AnalyzeFolder(cobolFile.FileReference.FilePath);

            // 3. Load all referenced files into memory and look for divisions inside each of them
            LoadCopyReferences(cobolFile);

            // 4. Analyze Variables (of file and all copys)
            AnalyzeVariables(cobolFile);

            // 5. Fix missing divisions
            FixMissingDivision(cobolFile);

            // 6. Build CobolTree
            if (cobolFile.DivisionsAndSection.AllDivisionsFound())
                BuildCobolTree(cobolFile);
        }

        /// <summary>
        /// Analyzes source text and source text of all copy references and fills the CobolFile.Variables property.
        /// </summary>
        private void AnalyzeVariables(CobolFile cobolFile)
        {
            cobolFile.Variables = VariablesUtil.Instance.AnalyzeVariables(cobolFile);

            foreach (var copyReference in cobolFile.CopyReferences.AsParallel())
            {
                copyReference.CobolFile.Variables = VariablesUtil.Instance.AnalyzeVariables(copyReference.CobolFile);
                foreach (var variablePair in copyReference.CobolFile.Variables)
                {
                    cobolFile.Variables.TryAdd(variablePair.Key, variablePair.Value);
                }
            }
        }

        /// <summary>
        /// Tries to find all four division statements and, for all that miss, tries to find them in copy references 
        /// and inserts them into the code.
        /// </summary>
        private static void FixMissingDivision(CobolFile cobolFile)
        {
            foreach (var copyReference in cobolFile.CopyReferences)
            {
                copyReference.CobolFile.DivisionsAndSection = TextUtil.Instance.FindDivisions(copyReference.CobolFile.Text);
            }

            if (!cobolFile.DivisionsAndSection.Identification.HasValue)
            {
                var missingCopy =
                    cobolFile.CopyReferences.FirstOrDefault(reference => reference.CobolFile.DivisionsAndSection.Identification.HasValue);
                TextUtil.Instance.Insert(cobolFile, missingCopy);
            }

            if (!cobolFile.DivisionsAndSection.Environment.HasValue)
            {
                var missingCopy =
                    cobolFile.CopyReferences.FirstOrDefault(reference => reference.CobolFile.DivisionsAndSection.Environment.HasValue);
                TextUtil.Instance.Insert(cobolFile, missingCopy);
            }

            if (!cobolFile.DivisionsAndSection.Data.HasValue)
            {
                var missingCopy =
                    cobolFile.CopyReferences.FirstOrDefault(reference => reference.CobolFile.DivisionsAndSection.Data.HasValue);
                TextUtil.Instance.Insert(cobolFile, missingCopy);
            }

            if (!cobolFile.DivisionsAndSection.Procedure.HasValue)
            {
                var missingCopy =
                    cobolFile.CopyReferences.FirstOrDefault(reference => reference.CobolFile.DivisionsAndSection.Procedure.HasValue);
                TextUtil.Instance.Insert(cobolFile, missingCopy);
            }
        }

        /// <summary>
        /// Finds all copy references, creates CobolFile-objects for each one and adds them to the CobolFile.CopyReferences List
        /// </summary>
        private void LoadCopyReferences(CobolFile cobolFile)
        {
            Logger.Info("Resolving copy references for file {0}.", cobolFile.Name);

            var refs = TextUtil.Instance.FindCopyReferences(cobolFile.Text);

            foreach (var copyReference in refs.AsParallel())
            {
                // Load text and create CobolFile
                copyReference.CobolFile = new CobolFileBuilder().Build(copyReference);

                // Look for Identification, Environment, Data and Procedure Divion
                copyReference.CobolFile.DivisionsAndSection = TextUtil.Instance.FindDivisions(copyReference.CobolFile.Text);

                // Add to parent's copy-list
                cobolFile.CopyReferences.Add(copyReference);
            }

            Logger.Info("Finished loading copy references for file {0}.", cobolFile.Name);
        }

        private void BuildCobolTree(CobolFile cobolFile)
        {
            try
            {
                Logger.Info("Building CobolTree for file {0}.", cobolFile.Name);

                var builder = new CobolTreeBuilder();
                cobolFile.CobolTree = builder.Build(cobolFile);
            }
            catch (Exception exception)
            {
                Logger.Error("Error building CobolTree: {0}.", exception.Message);
                MessageBox.Show(Resources.Error_CobolTree_could_not_be_built + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }
    }
}