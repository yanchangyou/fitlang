// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.settings.mappings;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonShortcuts;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.ui.MasterDetailsComponent;
import com.intellij.ui.EditorNotifications;
import com.intellij.util.Function;
import com.intellij.util.IconUtil;
import com.intellij.util.ThreeState;
import com.intellij.util.containers.MultiMap;
import fit.intellij.json.JsonBundle;
import fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration;
import fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import fit.jetbrains.jsonSchema.remote.JsonFileResolver;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.tree.DefaultTreeModel;
import java.io.File;
import java.util.*;

/**
 * @author Irina.Chernushina on 2/2/2016.
 */
public class JsonSchemaMappingsConfigurable extends MasterDetailsComponent implements SearchableConfigurable, Disposable {
  @NonNls public static final String SETTINGS_JSON_SCHEMA = "settings.json.schema";

  private final static Comparator<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> COMPARATOR = (o1, o2) -> {
    if (o1.isApplicationDefined() != o2.isApplicationDefined()) {
      return o1.isApplicationDefined() ? 1 : -1;
    }
    return o1.getName().compareToIgnoreCase(o2.getName());
  };
  static final String STUB_SCHEMA_NAME = "New Schema";
  private String myError;

  @NotNull
  private final Project myProject;
  private final TreeUpdater myTreeUpdater = showWarning -> {
    TREE_UPDATER.run();
    updateWarningText(showWarning);
  };

  private final Function<String, String> myNameCreator = s -> createUniqueName(s);

  public JsonSchemaMappingsConfigurable(@NotNull final Project project) {
    myProject = project;
    initTree();
  }

  @Nullable
  @Override
  protected String getEmptySelectionString() {
    return myRoot.children().hasMoreElements()
           ? fit.intellij.json.JsonBundle.message("schema.configuration.mapping.empty.area.string")
           : fit.intellij.json.JsonBundle.message("schema.configuration.mapping.empty.area.alt.string");
  }

  @Nullable
  @Override
  protected ArrayList<AnAction> createActions(boolean fromPopup) {
    final ArrayList<AnAction> result = new ArrayList<>();
    result.add(new DumbAwareAction(
      fit.intellij.json.JsonBundle.messagePointer("action.DumbAware.JsonSchemaMappingsConfigurable.text.add"),
      fit.intellij.json.JsonBundle.messagePointer("action.DumbAware.JsonSchemaMappingsConfigurable.description.add"),
      IconUtil.getAddIcon()) {
      {
        registerCustomShortcutSet(CommonShortcuts.INSERT, myTree);
      }
      @Override
      public void actionPerformed(@NotNull AnActionEvent e) {
        addProjectSchema();
      }
    });
    result.add(new MyDeleteAction());
    return result;
  }

  public fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration addProjectSchema() {
    fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration = new fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration(createUniqueName(STUB_SCHEMA_NAME),
                                                                                     JsonSchemaVersion.SCHEMA_4, "", false, null);
    addCreatedMappings(configuration);
    return configuration;
  }

  @SuppressWarnings("SameParameterValue")
  @NotNull
  private String createUniqueName(@NotNull String s) {
    int max = -1;
    Enumeration children = myRoot.children();
    while (children.hasMoreElements()) {
      Object element = children.nextElement();
      if (!(element instanceof MyNode)) continue;
      String displayName = ((MyNode)element).getDisplayName();
      if (displayName.startsWith(s)) {
        String lastPart = displayName.substring(s.length()).trim();
        if (lastPart.length() == 0 && max == -1) {
          max = 1;
          continue;
        }
        int i = tryParseInt(lastPart);
        if (i == -1) continue;
        max = Math.max(i, max);
      }
    }
    return max == -1 ? s : (s + " " + (max + 1));
  }

  private static int tryParseInt(@NotNull String s) {
    try {
      return Integer.parseInt(s);
    }
    catch (NumberFormatException e) {
      return -1;
    }
  }

  private void addCreatedMappings(@NotNull final fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration info) {
    final JsonSchemaConfigurable configurable = new JsonSchemaConfigurable(myProject, "", info, myTreeUpdater, myNameCreator);
    configurable.setError(myError, true);
    final MyNode node = new MyNode(configurable);
    addNode(node, myRoot);
    selectNodeInTree(node, true);
  }

  private void fillTree() {
    myRoot.removeAllChildren();

    if (myProject.isDefault()) return;

    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> list = getStoredList();
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration info : list) {
      String pathToSchema = info.getRelativePathToSchema();
      final JsonSchemaConfigurable configurable =
        new JsonSchemaConfigurable(myProject, JsonFileResolver.isAbsoluteUrl(pathToSchema) || new File(pathToSchema).isAbsolute() ? pathToSchema : new File(myProject.getBasePath(), pathToSchema).getPath(),
                                   info, myTreeUpdater, myNameCreator);
      configurable.setError(myError, true);
      myRoot.add(new MyNode(configurable));
    }
    ((DefaultTreeModel) myTree.getModel()).reload(myRoot);
    if (myRoot.children().hasMoreElements()) {
      myTree.addSelectionRow(0);
    }
  }

  @NotNull
  private List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> getStoredList() {
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> list = new ArrayList<>();
    final Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> projectState = fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration
      .getInstance(myProject).getStateMap();
    if (projectState != null) {
      list.addAll(projectState.values());
    }

    Collections.sort(list, COMPARATOR);
    return list;
  }

  @Override
  public void apply() throws ConfigurationException {
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> uiList = getUiList(true);
    validate(uiList);
    final Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> projectMap = new HashMap<>();
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration info : uiList) {
      projectMap.put(info.getName(), info);
    }

    JsonSchemaMappingsProjectConfiguration.getInstance(myProject).setState(projectMap);
    final Project[] projects = ProjectManager.getInstance().getOpenProjects();
    for (Project project : projects) {
      final fit.jetbrains.jsonSchema.ide.JsonSchemaService service = JsonSchemaService.Impl.get(project);
      if (service != null) service.reset();
    }
    DaemonCodeAnalyzer.getInstance(myProject).restart();
    EditorNotifications.getInstance(myProject).updateAllNotifications();
  }

  private static void validate(@NotNull List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> list) throws ConfigurationException {
    final Set<String> set = new HashSet<>();
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration info : list) {
      if (set.contains(info.getName())) {
        throw new ConfigurationException(fit.intellij.json.JsonBundle.message("schema.configuration.error.duplicate.name", info.getName()));
      }
      set.add(info.getName());
    }
  }

  @Override
  public boolean isModified() {
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> storedList = getStoredList();
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> uiList;
    try {
      uiList = getUiList(false);
    }
    catch (ConfigurationException e) {
      //will not happen
      return false;
    }
    return !storedList.equals(uiList);
  }

  private void updateWarningText(boolean showWarning) {
    final MultiMap<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item> patternsMap = new MultiMap<>();
    final StringBuilder sb = new StringBuilder();
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> list;
    try {
      list = getUiList(false);
    }
    catch (ConfigurationException e) {
      // will not happen
      return;
    }
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration info : list) {
      info.refreshPatterns();
      final JsonSchemaPatternComparator comparator = new JsonSchemaPatternComparator(myProject);
      final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item> patterns = info.getPatterns();
      for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item pattern : patterns) {
        for (Map.Entry<String, Collection<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item>> entry : patternsMap.entrySet()) {
          for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item item : entry.getValue()) {
            final ThreeState similar = comparator.isSimilar(pattern, item);
            if (ThreeState.NO.equals(similar)) continue;

            if (sb.length() > 0) sb.append('\n');
            sb.append(fit.intellij.json.JsonBundle.message("schema.configuration.error.conflicting.mappings.desc",
                                         pattern.getPresentation(),
                                         info.getName(),
                                         item.getPresentation(),
                                         entry.getKey()));
          }
        }
      }
      patternsMap.put(info.getName(), patterns);
    }
    if (sb.length() > 0) {
      myError = fit.intellij.json.JsonBundle.message("schema.configuration.error.conflicting.mappings.title", sb.toString());
    } else {
      myError = null;
    }
    final Enumeration children = myRoot.children();
    while (children.hasMoreElements()) {
      Object o = children.nextElement();
      if (o instanceof MyNode && ((MyNode)o).getConfigurable() instanceof JsonSchemaConfigurable) {
        ((JsonSchemaConfigurable) ((MyNode)o).getConfigurable()).setError(myError, showWarning);
      }
    }
  }

  public void selectInTree(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration) {
    final Enumeration children = myRoot.children();
    while (children.hasMoreElements()) {
      final MyNode node = (MyNode)children.nextElement();
      JsonSchemaConfigurable configurable = (JsonSchemaConfigurable)node.getConfigurable();
      if (Objects.equals(configurable.getUiSchema(), configuration)) {
        selectNodeInTree(node);
      }
    }
  }

  @NotNull
  private List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> getUiList(boolean applyChildren) throws ConfigurationException {
    final List<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> uiList = new ArrayList<>();
    final Enumeration children = myRoot.children();
    while (children.hasMoreElements()) {
      final MyNode node = (MyNode)children.nextElement();
      if (applyChildren) {
        node.getConfigurable().apply();
        uiList.add(getSchemaInfo(node));
      }
      else {
        uiList.add(((JsonSchemaConfigurable) node.getConfigurable()).getUiSchema());
      }
    }
    Collections.sort(uiList, COMPARATOR);
    return uiList;
  }

  @Override
  public void reset() {
    fillTree();
    updateWarningText(true);
  }

  @Override
  protected Comparator<MyNode> getNodeComparator() {
    return (o1, o2) -> {
      if (o1.getConfigurable() instanceof JsonSchemaConfigurable && o2.getConfigurable() instanceof JsonSchemaConfigurable) {
        return COMPARATOR.compare(getSchemaInfo(o1), getSchemaInfo(o2));
      }
      return o1.getDisplayName().compareToIgnoreCase(o2.getDisplayName());
    };
  }

  private static UserDefinedJsonSchemaConfiguration getSchemaInfo(@NotNull final MyNode node) {
    return ((JsonSchemaConfigurable) node.getConfigurable()).getSchema();
  }

  @Nls
  @Override
  public String getDisplayName() {
    return JsonBundle.message("configurable.JsonSchemaMappingsConfigurable.display.name");
  }


  @Override
  public void dispose() {
    final Enumeration children = myRoot.children();
    while (children.hasMoreElements()) {
      Object o = children.nextElement();
      if (o instanceof MyNode) {
        ((MyNode)o).getConfigurable().disposeUIResources();
      }
    }
  }

  @NotNull
  @Override
  public String getId() {
    return SETTINGS_JSON_SCHEMA;
  }

  @Override
  public String getHelpTopic() {
    return SETTINGS_JSON_SCHEMA;
  }
}
