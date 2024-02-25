// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.widget;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.ListPopup;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.jetbrains.jsonSchema.JsonSchemaCatalogProjectConfiguration;
import fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration;
import fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class JsonSchemaStatusPopup {
  static final fit.jetbrains.jsonSchema.extension.JsonSchemaInfo ADD_MAPPING = new fit.jetbrains.jsonSchema.extension.JsonSchemaInfo("") {
    @NotNull
    @Override
    public String getDescription() {
      return JsonBundle.message("schema.widget.add.mapping");
    }
  };

  static final fit.jetbrains.jsonSchema.extension.JsonSchemaInfo IGNORE_FILE = new fit.jetbrains.jsonSchema.extension.JsonSchemaInfo("") {

    @Nls
    @Override
    public @NotNull String getDescription() {
      return JsonBundle.message("schema.widget.no.mapping");
    }
  };

  static final fit.jetbrains.jsonSchema.extension.JsonSchemaInfo STOP_IGNORE_FILE = new fit.jetbrains.jsonSchema.extension.JsonSchemaInfo("") {

    @Nls
    @Override
    public @NotNull String getDescription() {
      return JsonBundle.message("schema.widget.stop.ignore.file");
    }
  };

  static final fit.jetbrains.jsonSchema.extension.JsonSchemaInfo EDIT_MAPPINGS = new fit.jetbrains.jsonSchema.extension.JsonSchemaInfo("") {
    @NotNull
    @Override
    public String getDescription() {
      return JsonBundle.message("schema.widget.edit.mappings");
    }
  };

  public static final fit.jetbrains.jsonSchema.extension.JsonSchemaInfo LOAD_REMOTE = new fit.jetbrains.jsonSchema.extension.JsonSchemaInfo("") {
    @NotNull
    @Override
    public String getDescription() {
      return JsonBundle.message("schema.widget.load.mappings");
    }
  };

  static ListPopup createPopup(@NotNull JsonSchemaService service,
                               @NotNull Project project,
                               @NotNull VirtualFile virtualFile,
                               boolean showOnlyEdit) {
    JsonSchemaInfoPopupStep step = createPopupStep(service, project, virtualFile, showOnlyEdit);
    return JBPopupFactory.getInstance().createListPopup(step);
  }

  @NotNull
  static JsonSchemaInfoPopupStep createPopupStep(@NotNull JsonSchemaService service,
                                                 @NotNull Project project,
                                                 @NotNull VirtualFile virtualFile,
                                                 boolean showOnlyEdit) {
    List<fit.jetbrains.jsonSchema.extension.JsonSchemaInfo> allSchemas;
    fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration configuration = JsonSchemaMappingsProjectConfiguration.getInstance(project);
    UserDefinedJsonSchemaConfiguration mapping = configuration.findMappingForFile(virtualFile);
    if (!showOnlyEdit || mapping == null) {
      List<fit.jetbrains.jsonSchema.extension.JsonSchemaInfo> infos = service.getAllUserVisibleSchemas();
      Comparator<fit.jetbrains.jsonSchema.extension.JsonSchemaInfo> comparator = Comparator.comparing(fit.jetbrains.jsonSchema.extension.JsonSchemaInfo::getDescription, String::compareToIgnoreCase);
      Stream<fit.jetbrains.jsonSchema.extension.JsonSchemaInfo> registered = infos.stream().filter(i -> i.getProvider() != null).sorted(comparator);
      List<JsonSchemaInfo> otherList = ContainerUtil.emptyList();

      if (JsonSchemaCatalogProjectConfiguration.getInstance(project).isRemoteActivityEnabled()) {
        otherList = infos.stream().filter(i -> i.getProvider() == null).sorted(comparator).collect(Collectors.toList());
        if (otherList.size() == 0) {
          otherList = ContainerUtil.createMaybeSingletonList(LOAD_REMOTE);
        }
      }
      allSchemas = Stream.concat(registered, otherList.stream()).collect(Collectors.toList());
      allSchemas.add(0, mapping == null ? ADD_MAPPING : EDIT_MAPPINGS);
    }
    else {
      allSchemas = new SmartList<>(EDIT_MAPPINGS);
    }

    if (configuration.isIgnoredFile(virtualFile)) {
      allSchemas.add(0, STOP_IGNORE_FILE);
    }
    else {
      allSchemas.add(0, IGNORE_FILE);
    }
    return new JsonSchemaInfoPopupStep(allSchemas, project, virtualFile, service, null);
  }
}
