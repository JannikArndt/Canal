namespace Canal.UserControls
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Drawing;
    using System.IO;
    using System.Linq;
    using System.Text.RegularExpressions;
    using System.Windows.Forms;
    using FastColoredTextBoxNS;
    using FastColoredTextBoxNS.Events;
    using Logging;
    using Model.File;
    using Model.References;
    using Properties;
    using Util;
    using WordInfoViews;

    /// <summary>
    /// User control that handles a <see cref="CobolFile"/> and the Editor
    /// </summary>
    public sealed partial class FileControl : UserControl
    {
        public FileControl(string filename, MainWindow parent)
        {
            InitializeComponent();
            Name = "FileControl";
            Dock = DockStyle.Fill;

            try
            {
                MainWindow = parent;

                CobolFile = new CobolFileBuilder().Build(filename);

                AnalyzeFileAsync();

                // event handlers
                VariableSelected += OnVariableSelected;
                SectionSelected += OnSectionSelected;
                CallReferenceSelected += OnCallReferenceSelected;
                ProcedureSelected += OnProcedureSelected;

                searchPrecise.Checked = true;
                searchFuzzy.Click += HandleSearchKindChanged;
                searchRegEx.Click += HandleSearchKindChanged;
                searchPrecise.Click += HandleSearchKindChanged;

                // initialize FastColoredTextBox
                codeBox.Font = SourceCodePro.Instance.Regular();
                codeBox.SetTextAsync(CobolFile.Text);

                codeBox.KeyDown += HandleKeyDown;
                codeBox.WordSelected += CodeBoxOnWordSelected;
                codeBox.UndoRedoStateChanged += CodeBoxOnUndoRedoStateChanged;
                codeBox.VisualMarkerClick += CodeBoxOnPerformMarkerClick;
                codeBox.SelectionChanged += CodeBoxOnSelectionChanged;
                codeBox.TextChanged += (sender, args) =>
                {
                    if (!UnsavedChanges && SavedVersionChanged != null)
                    {
                        UnsavedChanges = true;
                        SavedVersionChanged(sender, args);
                    }

                    saveButton.Enabled = true;
                };

                searchBox.Text = Resources.SearchPlaceholder;
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error initializing file control for CobolFile {0}: {1}", filename, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        /// <summary>
        /// Event is thrown the first time the text is changed after it is loaded or saved.
        /// </summary>
        public event EventHandler<TextChangedEventArgs> SavedVersionChanged;

        public event EventHandler<FileSystemEventArgs> FileSaved;

        public event EventHandler<Variable> VariableSelected;

        public event EventHandler<Section> SectionSelected;

        public event EventHandler<FileReference> CallReferenceSelected;

        public event EventHandler<Tuple<Procedure, string>> ProcedureSelected;

        /// <summary>
        /// Shows if there are unsaved changes in the text.
        /// </summary>
        public bool UnsavedChanges { get; private set; }

        public CobolFile CobolFile { get; private set; }

        public MainWindow MainWindow { get; private set; }

        public void AnalyzeFileAsync()
        {
            if (CobolFile.FileReference == null) return;

            var worker = new BackgroundWorker();
            worker.DoWork += new Analyzer().AnalyzeFile;
            worker.RunWorkerCompleted += CheckPlausability;
            worker.RunWorkerCompleted += InitTabs;
            worker.RunWorkerCompleted += SetFoldingMarkers;
            worker.RunWorkerAsync(CobolFile);
        }

        private void CheckPlausability(object sender, RunWorkerCompletedEventArgs runWorkerCompletedEventArgs)
        {
            if (CobolFile.DivisionsAndSection.ProcedureMissing())
            {
                var result = MessageBox.Show(Resources.ProcedureDivisionMissing_Text, Resources.ProcedureDivisionMissing_Title, MessageBoxButtons.YesNo);
                if (result == DialogResult.Yes)
                    InsertCopybooksIntoSource();
            }
        }

        private void SetFoldingMarkers(object sender, RunWorkerCompletedEventArgs runWorkerCompletedEventArgs)
        {
            try
            {
                var foldingAreas = new List<CobolTreeNode>(CobolFile.CobolTree.GetAllDivisions())
                    .Concat(CobolFile.CobolTree.GetAllSections())
                    .Concat(CobolFile.CobolTree.GetAllProcedures());

                foreach (var area in foldingAreas)
                {
                    codeBox.SetFoldingMarker(area.StartIndex, area.EndIndex, area.Name);
                }
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error setting folding markers: {0}.", exception.Message);
            }
        }

        private void CodeBoxOnPerformMarkerClick(object sender, VisualMarkerEventArgs visualMarkerEventArgs)
        {
            var clickedLineText = visualMarkerEventArgs.Marker.Text;

            codeBox.FindNext(clickedLineText, false, false, false, true);

            var performMatch = Regex.Match(clickedLineText, Constants.Perform, RegexOptions.IgnoreCase);
            if (performMatch.Success)
            {
                codeBox.FindNext(@"^.{7}" + performMatch.Groups[1].Value + @"(\.| +USING| SECTION\.)", false, true, false, true);
                return;
            }

            var gotoMatch = Regex.Match(clickedLineText, Constants.GoTo, RegexOptions.IgnoreCase);
            if (gotoMatch.Success)
            {
                codeBox.FindNext(@"^.{7}" + gotoMatch.Groups[1].Value + @"(\.| +USING)", false, true, false, true);
                return;
            }

            var callmatch = Regex.Match(clickedLineText, Constants.Call, RegexOptions.IgnoreCase);
            if (callmatch.Success)
            {
                var fileRef = FileUtil.Instance.GetFileReferences(callmatch.Groups[4].Value);
                if (fileRef.Count == 1)
                    MainWindow.OpenFile(fileRef.First().FilePath);
                else if (fileRef.Count > 0)
                    MessageBox.Show(Resources.MultipleFilesMatchSearch, Resources.MultipleFilesFound, MessageBoxButtons.OK);
                else
                    MessageBox.Show(Resources.FileCouldNotBeFound, Resources.FileNotFound, MessageBoxButtons.OK);
            }
        }

        public override string ToString()
        {
            return string.Format("FileControl: {0}", CobolFile.Name);
        }

        public bool HasFileReference()
        {
            return CobolFile.FileReference != null;
        }

        public string ExportToHtml()
        {
            return codeBox.Html;
        }

        public void RevertChanges()
        {
            codeBox.SetTextAsync(CobolFile.Text);
        }

        public void Save(string filename = "")
        {
            if (!string.IsNullOrWhiteSpace(filename))
                CobolFile.FileReference = new FileReference(filename);

            try
            {
                File.WriteAllText(CobolFile.FileReference.FilePath, codeBox.Text);
                CobolFile.Text = codeBox.Text;
                UnsavedChanges = false;
                saveButton.Enabled = false;
                if (FileSaved != null)
                    FileSaved(this, new FileSystemEventArgs(WatcherChangeTypes.Changed, CobolFile.FileReference.Directory, CobolFile.FileReference.ProgramName));
            }
            catch (IOException exception)
            {
                MessageBox.Show(Resources.FileControl_Save_Error_saving_file + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        public void FindInCodeBox(string pattern, bool matchCase, bool regex, bool wholeWord, bool firstSearch = false)
        {
            codeBox.FindNext(pattern, matchCase, regex, wholeWord, firstSearch);
        }

        public void FindInCodeBoxAfterGivenString(string word, string searchFromHere)
        {
            searchFromHere = Regex.Replace(searchFromHere, "[^a-zA-Z0-9 \\*]", "[^a-zA-Z0-9 ]");
            searchFromHere = Regex.Replace(searchFromHere, " ", " *");
            codeBox.FindNext(searchFromHere, false, true, false, true);
            codeBox.SelectionStart = codeBox.SelectionStart - codeBox.SelectionLength;
            codeBox.ClearSelected();
            codeBox.FindNext(word, false, false, false);
        }

        private void InitTabs(object sender, RunWorkerCompletedEventArgs runWorkerCompletedEventArgs)
        {
            if (IsDisposed)
            {
                Logger.Warning("Tried to initialize tabs on disposed FileControl.");
                return;
            }

            try
            {
                tableOfContents.SetCobolFile(CobolFile);
                tableOfContents.OnWordSelected += CodeBoxOnWordSelected;
                ShowProgramInfo();
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error initializing tabs for CobolFile {0}: {1}", CobolFile.Name, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        private void ShowProgramInfo()
        {
            DisposeAndClear(splitContainerRight.Panel2.Controls);
            var fileInfoControl2 = new ProgramInfo(CobolFile, this) { Dock = DockStyle.Fill };
            splitContainerRight.Panel2.Controls.Add(fileInfoControl2);
        }

        private void HandleKeyDown(object sender, KeyEventArgs e)
        {
            try
            {
                if (e.Shift)
                    switch (e.KeyCode)
                    {
                        case Keys.F3:
                            TrySearch(false, true);
                            return;
                    }
                else if (e.Control)
                {
                    switch (e.KeyCode)
                    {
                        case Keys.F:
                            searchBox.Focus();
                            return;
                    }
                }
                else
                    switch (e.KeyCode)
                    {
                        case Keys.F3:
                            TrySearch(false);
                            return;
                        case Keys.Escape:
                            searchBox.Tag = false;
                            searchBox.Text = string.Empty;
                            searchBox.Tag = true;
                            return;
                    }
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error processing key event {0} with KeyCode {1}: {2}.", e.ToString(), e.KeyCode, exception.Message);
            }
        }

        #region Code Box Events

        /// <summary>
        /// Handles the Event if a word is selected and splits it up into the separate Events for 
        /// <see cref="VariableSelected"/>, <see cref="ProcedureSelected"/>, <see cref="SectionSelected"/>
        /// and <see cref="CallReferenceSelected"/>.
        /// </summary>
        private void CodeBoxOnWordSelected(object sender, WordSelectedEventArgs eventArgs)
        {
            try
            {
                Logger.Info("Selected word {0}", eventArgs.Word);

                // 1. No CobolFile? Nothing to do.
                if (CobolFile == null)
                    return;

                // 2. No CobolTree? Show Program infos
                if (CobolFile.CobolTree == null)
                {
                    ShowProgramInfo();
                    return;
                }

                // 3. Is the word a variable?
                if (CobolFile.Variables.ContainsKey(eventArgs.Word) && VariableSelected != null)
                {
                    VariableSelected(sender, CobolFile.Variables[eventArgs.Word]);
                    return;
                }

                // 4. Is the word a procedure?
                var procedure = CobolFile.CobolTree.GetAllProcedures().FirstOrDefault(proc => proc.Name == eventArgs.Word);
                if (procedure != null && ProcedureSelected != null)
                {
                    ProcedureSelected(sender, new Tuple<Procedure, string>(procedure, eventArgs.LookFor));
                    return;
                }

                // 5. Is it a section?
                var section = CobolFile.CobolTree.GetAllSections().FirstOrDefault(sec => sec.Name == eventArgs.Word);
                if (section != null && SectionSelected != null)
                {
                    SectionSelected(sender, section);
                    return;
                }

                // 6. Call Reference? Open file, keep Info
                var callRef = CobolFile.CobolTree.CallReferences.FirstOrDefault(call => call.ProgramName == eventArgs.Word);
                if (callRef != null && CallReferenceSelected != null)
                {
                    CallReferenceSelected(sender, callRef);
                    return;
                }

                ShowProgramInfo();
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error trying to show selected word {0}: {1}.", eventArgs.Word, exception.Message);
            }
        }

        private void OnProcedureSelected(object sender, Tuple<Procedure, string> info)
        {
            DisposeAndClear(splitContainerRight.Panel2.Controls);

            var procedureInfoControl = new ProcedureInfo(info.Item1, this) { Dock = DockStyle.Fill, VerticalScroll = { Enabled = true } };
            splitContainerRight.Panel2.Controls.Add(procedureInfoControl);

            procedureInfoControl.OnProcedureSelected += CodeBoxOnWordSelected;

            if (sender == codeBox)
                return;

            codeBox.FindNext(@"^.{7}" + info.Item1.Name + @"(\.| +USING| OF)", false, true, false, true);
            if (!string.IsNullOrWhiteSpace(info.Item2))
                codeBox.FindNext(@"PERFORM +" + info.Item2, false, true, false); // NOT first search
        }

        private void OnCallReferenceSelected(object sender, FileReference fileReference)
        {
            MainWindow.OpenFile(fileReference.FilePath);
        }

        private void OnSectionSelected(object sender, Section section)
        {
            if (sender.GetType() == typeof(TableOfContents))
                codeBox.FindNext(@"^.{7}" + section.Name + @"\.", false, true, false, true);
        }

        private void OnVariableSelected(object sender, Variable variable)
        {
            DisposeAndClear(splitContainerRight.Panel2.Controls);

            var variableInfoControl = new VariableInfo(variable, this) { Dock = DockStyle.Fill };
            splitContainerRight.Panel2.Controls.Add(variableInfoControl);
            variableInfoControl.ScrollToSelectedVariable();

            if (sender.GetType() == typeof(TableOfContents))
                codeBox.FindNext(variable.VariableName, false, false, true, true);
        }

        /// <summary>
        /// Disposes all items in the List and then clears the list
        /// </summary>
        private static void DisposeAndClear(ControlCollection controlCollection)
        {
            foreach (Control control in controlCollection)
                control.Dispose();

            controlCollection.Clear();
        }

        private void CodeBoxOnUndoRedoStateChanged(object sender, EventArgs eventArgs)
        {
            undoButton.Enabled = codeBox.UndoEnabled;
            redoButton.Enabled = codeBox.RedoEnabled;
        }

        private void CodeBoxOnSelectionChanged(object sender, EventArgs eventArgs)
        {
            tableOfContents.HighlightFirstMatchingNode(FindLastProcedureOrSectionOrDivisonNames(codeBox.Selection.FromLine));
        }

        /// <summary>
        /// Filters all possible procedure, section and division names from the given line up to the start of the document.
        /// This has to return a list of all possible names instead of just the next name as there is no way of finding a 
        /// regex which distincly filters named names only. This list can be compared to the ToC later and the first match equals the contextual correct name. 
        /// </summary>
        /// <param name="lineNumber">The line number to search up from.</param>
        /// <returns>A list of possible procedure, section and division names from the given line up.</returns>
        private List<string> FindLastProcedureOrSectionOrDivisonNames(int lineNumber)
        {
            var procedureNames = new List<string>();
            for (int currentLineNumber = lineNumber; currentLineNumber >= 0; currentLineNumber--)
            {
                var currenctLineText = codeBox.GetLineText(currentLineNumber);
                var match = Constants.ProcedureOrSectionOrDivisonRegex.Match(currenctLineText).Groups["name"].Value;
                if (!string.IsNullOrEmpty(match))
                    procedureNames.Add(match);
            }

            return procedureNames;
        }

        #endregion

        #region Search Box

        private void TrySearch(bool firstSearch, bool reverse = false)
        {
            if (!(bool)searchBox.Tag) return;

            try
            {
                searchBox.BackColor = SystemColors.Window;
                codeBox.FindNext(GetSearchQuery(), false, SearchWithRegEx(), false, firstSearch, reverse);
            }
            catch (ArgumentException)
            {
                // aparently the only way to validate a regex
                searchBox.BackColor = Color.PaleVioletRed;
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error trying to search for {0}: {1}.", searchBox.Text, exception.GetType());
            }
        }

        private string GetSearchQuery()
        {
            if (!searchFuzzy.Checked)
                return searchBox.Text;

            var queryAsRegex = Regex.Replace(searchBox.Text, "[^a-zA-Z0-9 \\*]", "[^a-zA-Z0-9 ]");
            queryAsRegex = Regex.Replace(queryAsRegex, " ", " *");
            return queryAsRegex;
        }

        private bool SearchWithRegEx()
        {
            return searchRegEx.Checked || searchFuzzy.Checked;
        }

        private void SeachBoxTextChanged(object sender, EventArgs e)
        {
            TrySearch(true);
        }

        private void SearchBoxEnter(object sender, EventArgs e)
        {
            var box = (ToolStripTextBox)sender;
            if (box.Text == Resources.SearchPlaceholder)
            {
                box.Tag = false;
                box.Text = "";
                box.Tag = true;
            }
        }

        private void SearchBoxLeave(object sender, EventArgs e)
        {
            var box = (ToolStripTextBox)sender;
            if (string.IsNullOrWhiteSpace(box.Text))
            {
                box.Tag = false;
                box.Text = Resources.SearchPlaceholder;
                box.Tag = true;
            }
        }

        private void HandleSearchKindChanged(object sender, EventArgs eventArgs)
        {
            searchFuzzy.Checked = false;
            searchPrecise.Checked = false;
            searchRegEx.Checked = false;
            ((ToolStripButton)sender).Checked = true;
        }

        #endregion

        #region Button Clicks

        private void NewButton_Click(object sender, EventArgs e)
        {
            MainWindow.New();
        }

        private void OpenButton_Click(object sender, EventArgs e)
        {
            MainWindow.OpenFile();
        }

        private void SaveButton_Click(object sender, EventArgs e)
        {
            MainWindow.Save();
        }

        private void FormatCodeButton_Click(object sender, EventArgs e)
        {
            var selection = new Range(codeBox, codeBox.Selection.Start, codeBox.Selection.End);
            if (codeBox.Selection.IsEmpty)
                codeBox.SelectAll();
            codeBox.DoAutoIndent();
            codeBox.Selection = selection;
        }

        private void NavigateBackwardClick(object sender, EventArgs e)
        {
            codeBox.NavigateBackward();
            navigateForwardButton.Enabled = true;
        }

        private void NavigateForwardButtonClick(object sender, EventArgs e)
        {
            var success = codeBox.NavigateForward();
            if (!success)
                navigateForwardButton.Enabled = false;
        }

        private void UndoButton_Click(object sender, EventArgs e)
        {
            codeBox.Undo();
        }

        private void RedoButton_Click(object sender, EventArgs e)
        {
            codeBox.Redo();
        }

        private void FindNextButtonClick(object sender, EventArgs e)
        {
            TrySearch(false);
        }

        private void FindPreviousButtonClick(object sender, EventArgs e)
        {
            TrySearch(false, true);
        }

        #endregion

        #region Functions

        /// <summary>
        /// Displays a filter window and then insert the selected copybook-references into the text and refreshes the view
        /// </summary>
        public void InsertCopybooksIntoSource()
        {
            var worker = new BackgroundWorker();

            try
            {
                var references = TextUtil.Instance.FindCopyReferences(CobolFile.Text).ToList();

                var filter = new FileReferenceFilter(references)
                {
                    Text = Resources.FilterCopybooksWindowTitle
                };
                filter.ShowDialog();

                if (filter.Result != null && filter.Result.Any())
                {
                    worker.DoWork += (sender, args) =>
                    {
                        foreach (var copyRef in filter.Result)
                        {
                            TextUtil.Instance.Insert(CobolFile, copyRef);
                        }
                    };

                    worker.RunWorkerCompleted += (sender, args) => codeBox.SetTextAsync(CobolFile.Text);
                    worker.RunWorkerCompleted += (sender, args) =>
                    {
                        var result = MessageBox.Show(Resources.CodeHasChanged_Text, Resources.CodeHasChanged_Title, MessageBoxButtons.YesNo);
                        if (result == DialogResult.Yes)
                            AnalyzeFileAsync();
                    };

                    worker.RunWorkerAsync();
                }
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error trying to insert copybooks into source: {0}.", exception.Message);
                MessageBox.Show(
                    string.Format("Error trying to insert copybooks into source: {0}.", exception.Message),
                    Resources.Error,
                    MessageBoxButtons.OK);
            }
        }

        #endregion
    }
}
