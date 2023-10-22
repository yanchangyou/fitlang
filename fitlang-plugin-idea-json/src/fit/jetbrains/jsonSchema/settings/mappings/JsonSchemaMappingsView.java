// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.settings.mappings;

import com.intellij.icons.AllIcons;
import com.intellij.ide.util.PsiNavigationSupport;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.CommonShortcuts;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.FixedSizeButton;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.ui.panel.ComponentPanelBuilder;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.BalloonBuilder;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.AnActionButton;
import com.intellij.ui.AnActionButtonRunnable;
import com.intellij.ui.DocumentAdapter;
import com.intellij.ui.ToolbarDecorator;
import com.intellij.ui.awt.RelativePoint;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.table.TableView;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.ui.*;
import fit.intellij.json.JsonBundle;
import fit.jetbrains.jsonSchema.JsonMappingKind;
import fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import fit.jetbrains.jsonSchema.remote.JsonFileResolver;
import fit.jetbrains.jsonSchema.widget.JsonSchemaInfoPopupStep;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import java.awt.*;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;

/**
 * @author Irina.Chernushina on 2/2/2016.
 */
public class JsonSchemaMappingsView implements Disposable {
  private static final String ADD_SCHEMA_MAPPING = "settings.json.schema.add.mapping";
  private static final String EDIT_SCHEMA_MAPPING = "settings.json.schema.edit.mapping";
  private static final String REMOVE_SCHEMA_MAPPING = "settings.json.schema.remove.mapping";
  private final TreeUpdater myTreeUpdater;
  private final BiConsumer<? super String, ? super Boolean> mySchemaPathChangedCallback;
  private TableView<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item> myTableView;
  private JComponent myComponent;
  private Project myProject;
  private TextFieldWithBrowseButton mySchemaField;
  private ComboBox<fit.jetbrains.jsonSchema.impl.JsonSchemaVersion> mySchemaVersionComboBox;
  private JEditorPane myError;
  private String myErrorText;
  private JBLabel myErrorIcon;
  private boolean myInitialized;

  public JsonSchemaMappingsView(Project project,
                                TreeUpdater treeUpdater,
                                BiConsumer<? super String, ? super Boolean> schemaPathChangedCallback) {
    myTreeUpdater = treeUpdater;
    mySchemaPathChangedCallback = schemaPathChangedCallback;
    createUI(project);
  }

  private void createUI(final Project project) {
    myProject = project;
    MyAddActionButtonRunnable addActionButtonRunnable = new MyAddActionButtonRunnable();

    myTableView = new JsonMappingsTableView(addActionButtonRunnable);
    myTableView.getTableHeader().setVisible(false);
    final ToolbarDecorator decorator = ToolbarDecorator.createDecorator(myTableView);
    final MyEditActionButtonRunnableImpl editAction = new MyEditActionButtonRunnableImpl();
    decorator.setRemoveAction(new MyRemoveActionButtonRunnable())
             .setRemoveActionName(REMOVE_SCHEMA_MAPPING)
             .setAddAction(addActionButtonRunnable)
             .setAddActionName(fit.intellij.json.JsonBundle.message(ADD_SCHEMA_MAPPING))
             .setEditAction(editAction)
             .setEditActionName(fit.intellij.json.JsonBundle.message(EDIT_SCHEMA_MAPPING))
             .disableUpDownActions();

    JBTextField schemaFieldBacking = new JBTextField();
    mySchemaField = new TextFieldWithBrowseButton(schemaFieldBacking);
    mySchemaField.setButtonIcon(AllIcons.General.OpenDiskHover);
    FixedSizeButton urlButton = new FixedSizeButton();
    urlButton.setIcon(AllIcons.General.Web);
    urlButton.addActionListener(a -> {
      final fit.jetbrains.jsonSchema.ide.JsonSchemaService service = JsonSchemaService.Impl.get(myProject);
      List<fit.jetbrains.jsonSchema.extension.JsonSchemaInfo> schemas = service.getAllUserVisibleSchemas();
      JBPopupFactory.getInstance().createListPopup(new JsonSchemaInfoPopupStep(schemas,
                                                                               myProject, null, service, fit.intellij.json.JsonBundle.message("schema.configuration.mapping.remote")) {
        @Override
        protected void setMapping(@Nullable fit.jetbrains.jsonSchema.extension.JsonSchemaInfo selectedValue, @Nullable VirtualFile virtualFile, @NotNull Project project) {
          if (selectedValue != null) {
            mySchemaField.setText(selectedValue.getUrl(myProject));
            mySchemaPathChangedCallback.accept(selectedValue.getDescription(), true); // force updating name
          }
        }
      }).showInCenterOf(urlButton);
    });
    SwingHelper.installFileCompletionAndBrowseDialog(myProject, mySchemaField, fit.intellij.json.JsonBundle.message("json.schema.add.schema.chooser.title"),
                                                     FileChooserDescriptorFactory.createSingleFileDescriptor());
    mySchemaField.getTextField().getDocument().addDocumentListener(new DocumentAdapter() {
      @Override
      protected void textChanged(@NotNull DocumentEvent e) {
        mySchemaPathChangedCallback.accept(mySchemaField.getText(), false);
      }
    });
    attachNavigateToSchema();
    myError = SwingHelper.createHtmlLabel(fit.intellij.json.JsonBundle.message("json.schema.conflicting.mappings"), null, s -> {
      final BalloonBuilder builder = JBPopupFactory.getInstance().
        createHtmlTextBalloonBuilder(myErrorText, UIUtil.getBalloonWarningIcon(), MessageType.WARNING.getPopupBackground(), null);
      builder.setDisposable(this);
      builder.setHideOnClickOutside(true);
      builder.setCloseButtonEnabled(true);
      builder.createBalloon().showInCenterOf(myError);
    });

    JPanel schemaSelector = new JPanel(new BorderLayout());
    schemaSelector.add(mySchemaField, BorderLayout.CENTER);
    schemaSelector.add(urlButton, BorderLayout.EAST);

    final FormBuilder builder = FormBuilder.createFormBuilder();
    final JBLabel label = new JBLabel(fit.intellij.json.JsonBundle.message("json.schema.file.selector.title"));
    builder.addLabeledComponent(label, schemaSelector);
    label.setLabelFor(schemaSelector);
    label.setBorder(JBUI.Borders.empty(0, 10));
    schemaSelector.setBorder(JBUI.Borders.emptyRight(10));
    JBLabel versionLabel = new JBLabel(fit.intellij.json.JsonBundle.message("json.schema.version.selector.title"));
    mySchemaVersionComboBox = new ComboBox<>(new DefaultComboBoxModel<>(fit.jetbrains.jsonSchema.impl.JsonSchemaVersion.values()));
    versionLabel.setLabelFor(mySchemaVersionComboBox);
    versionLabel.setBorder(JBUI.Borders.empty(0, 10));
    builder.addLabeledComponent(versionLabel, mySchemaVersionComboBox);
    final JPanel wrapper = new JPanel(new BorderLayout());
    wrapper.setBorder(JBUI.Borders.empty(0, 10));
    myErrorIcon = new JBLabel(UIUtil.getBalloonWarningIcon());
    wrapper.add(myErrorIcon, BorderLayout.WEST);
    wrapper.add(myError, BorderLayout.CENTER);
    builder.addComponent(wrapper);
    JPanel panel = decorator.createPanel();
    panel.setBorder(BorderFactory.createCompoundBorder(JBUI.Borders.empty(0, 8), panel.getBorder()));
    builder.addComponentFillVertically(panel, 5);
    JLabel commentComponent = ComponentPanelBuilder.createCommentComponent(
      fit.intellij.json.JsonBundle.message("path.to.file.or.directory.relative.to.project.root.or.file.name"), false);
    commentComponent.setBorder(JBUI.Borders.empty(0, 8, 5, 0));
    builder.addComponent(commentComponent);

    myComponent = builder.getPanel();
  }

  @Override
  public void dispose() {
  }

  public void setError(final String text, boolean showWarning) {
    myErrorText = text;
    myError.setVisible(showWarning && text != null);
    myErrorIcon.setVisible(showWarning && text != null);
  }

  private void attachNavigateToSchema() {
    DumbAwareAction.create(e -> {
      String pathToSchema = mySchemaField.getText();
      if (StringUtil.isEmptyOrSpaces(pathToSchema) || JsonFileResolver.isHttpPath(pathToSchema)) return;
      VirtualFile virtualFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(new File(pathToSchema));
      if (virtualFile == null) {
        BalloonBuilder balloonBuilder = JBPopupFactory.getInstance()
          .createHtmlTextBalloonBuilder(JsonBundle.message("json.schema.file.not.found"), UIUtil.getBalloonErrorIcon(), MessageType.ERROR.getPopupBackground(), null);
        Balloon balloon = balloonBuilder.setFadeoutTime(TimeUnit.SECONDS.toMillis(3)).createBalloon();
        balloon.showInCenterOf(mySchemaField);
        return;
      }
      PsiNavigationSupport.getInstance().createNavigatable(myProject, virtualFile, -1).navigate(true);
    }).registerCustomShortcutSet(CommonShortcuts.getEditSource(), mySchemaField);
  }

  public List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item> getData() {
    return Collections.unmodifiableList(
      ContainerUtil
        .filter(myTableView.getListTableModel().getItems(), i -> i.mappingKind == fit.jetbrains.jsonSchema.JsonMappingKind.Directory || !StringUtil.isEmpty(i.path)));
  }

  public void setItems(String schemaFilePath,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaVersion version,
                       final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item> data) {
    myInitialized = true;
    mySchemaField.setText(schemaFilePath);
    mySchemaVersionComboBox.setSelectedItem(version);
    myTableView.setModelAndUpdateColumns(
      new ListTableModel<>(createColumns(), new ArrayList<>(data)));
  }

  public boolean isInitialized() {
    return myInitialized;
  }

  public fit.jetbrains.jsonSchema.impl.JsonSchemaVersion getSchemaVersion() {
    return (JsonSchemaVersion)mySchemaVersionComboBox.getSelectedItem();
  }

  public String getSchemaSubPath() {
    String schemaFieldText = mySchemaField.getText();
    if (JsonFileResolver.isAbsoluteUrl(schemaFieldText)) return schemaFieldText;
    return FileUtil.toSystemDependentName(JsonSchemaInfo.getRelativePath(myProject, schemaFieldText));
  }

  private ColumnInfo[] createColumns() {
    return new ColumnInfo[] { new MappingItemColumnInfo() };
  }

  public JComponent getComponent() {
    return myComponent;
  }

  private class MappingItemColumnInfo extends ColumnInfo<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item, String> {
    MappingItemColumnInfo() {super("");}

    @Nullable
    @Override
    public String valueOf(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item item) {
      return item.getPresentation();
    }

    @NotNull
    @Override
    public TableCellRenderer getRenderer(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item item) {
      return new DefaultTableCellRenderer() {
        @Override
        public Component getTableCellRendererComponent(JTable table,
                                                       Object value,
                                                       boolean isSelected,
                                                       boolean hasFocus,
                                                       int row,
                                                       int column) {
          JLabel label = (JLabel)super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
          label.setIcon(item.mappingKind.getIcon());

          String error = item.getError();
          if (error == null) {
            return label;
          }

          JPanel panel = new JPanel();
          panel.setLayout(new BorderLayout());
          panel.add(label, BorderLayout.CENTER);
          JLabel warning = new JLabel(AllIcons.General.Warning);
          panel.setBackground(label.getBackground());
          panel.setToolTipText(error);
          panel.add(warning, BorderLayout.LINE_END);
          return panel;
        }
      };
    }

    @Nullable
    @Override
    public TableCellEditor getEditor(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item item) {
      return new fit.jetbrains.jsonSchema.settings.mappings.JsonMappingsTableCellEditor(item, myProject, myTreeUpdater);
    }

    @Override
    public boolean isCellEditable(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item item) {
      return true;
    }
  }

  class MyAddActionButtonRunnable implements AnActionButtonRunnable {
    MyAddActionButtonRunnable() {
      super();
    }

    @Override
    public void run(AnActionButton button) {
      RelativePoint point = button.getPreferredPopupPoint();
      if (point == null) {
        point = new RelativePoint(button.getContextComponent(), new Point(0, 0));
      }
      JBPopupFactory.getInstance().createListPopup(new BaseListPopupStep<fit.jetbrains.jsonSchema.JsonMappingKind>(null,
                                                                                          fit.jetbrains.jsonSchema.JsonMappingKind.values()) {
        @NotNull
        @Override
        public String getTextFor(fit.jetbrains.jsonSchema.JsonMappingKind value) {
          return "Add " + StringUtil.capitalizeWords(value.getDescription(), true);
        }

        @Override
        public Icon getIconFor(fit.jetbrains.jsonSchema.JsonMappingKind value) {
          return value.getIcon();
        }

        @Override
        public PopupStep onChosen(fit.jetbrains.jsonSchema.JsonMappingKind selectedValue, boolean finalChoice) {
          if (finalChoice) {
            return doFinalStep(() -> doRun(selectedValue));
          }
          return PopupStep.FINAL_CHOICE;
        }
      }).show(point);
    }

    void doRun(JsonMappingKind mappingKind) {
      fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item currentItem = new UserDefinedJsonSchemaConfiguration.Item("", mappingKind);
      myTableView.getListTableModel().addRow(currentItem);
      myTableView.editCellAt(myTableView.getListTableModel().getRowCount() - 1, 0);

      myTreeUpdater.updateTree(false);
    }
  }

  private class MyEditActionButtonRunnableImpl implements AnActionButtonRunnable {
    MyEditActionButtonRunnableImpl() {
      super();
    }

    @Override
    public void run(AnActionButton button) {
      execute();
    }

    public void execute() {
      int selectedRow = myTableView.getSelectedRow();
      if (selectedRow == -1) return;
      myTableView.editCellAt(selectedRow, 0);
    }
  }

  private class MyRemoveActionButtonRunnable implements AnActionButtonRunnable {
    @Override
    public void run(AnActionButton button) {
      final int[] rows = myTableView.getSelectedRows();
      if (rows != null && rows.length > 0) {
        int cnt = 0;
        for (int row : rows) {
          myTableView.getListTableModel().removeRow(row - cnt);
          ++cnt;
        }
        myTableView.getListTableModel().fireTableDataChanged();
        myTreeUpdater.updateTree(true);
      }
    }
  }
}
