// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.widget;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.ListPopup;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.JsonSchemaCatalogProjectConfiguration;
import fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration;
import fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import org.jetbrains.annotations.NotNull;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JsonSchemaStatusPopup {
  static final JsonSchemaInfo ADD_MAPPING = new JsonSchemaInfo("") {
    @NotNull
    @Override
    public String getDescription() {
      return JsonBundle.message("schema.widget.add.mapping");
    }
  };

  static final JsonSchemaInfo EDIT_MAPPINGS = new JsonSchemaInfo("") {
    @NotNull
    @Override
    public String getDescription() {
      return JsonBundle.message("schema.widget.edit.mappings");
    }
  };

  public static final JsonSchemaInfo LOAD_REMOTE = new JsonSchemaInfo("") {
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
    fit.jetbrains.jsonSchema.widget.JsonSchemaInfoPopupStep step = createPopupStep(service, project, virtualFile, showOnlyEdit);
    return JBPopupFactory.getInstance().createListPopup(step);
  }

  @NotNull
  static fit.jetbrains.jsonSchema.widget.JsonSchemaInfoPopupStep createPopupStep(@NotNull JsonSchemaService service,
                                                                                 @NotNull Project project,
                                                                                 @NotNull VirtualFile virtualFile,
                                                                                 boolean showOnlyEdit) {
    List<JsonSchemaInfo> allSchemas;
    JsonSchemaMappingsProjectConfiguration configuration = JsonSchemaMappingsProjectConfiguration.getInstance(project);
    UserDefinedJsonSchemaConfiguration mapping = configuration.findMappingForFile(virtualFile);
    if (!showOnlyEdit || mapping == null) {
      List<JsonSchemaInfo> infos = service.getAllUserVisibleSchemas();
      Comparator<JsonSchemaInfo> comparator = Comparator.comparing(JsonSchemaInfo::getDescription, String::compareTo);
      Stream<JsonSchemaInfo> registered = infos.stream().filter(i -> i.getProvider() != null).sorted(comparator);
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
      allSchemas = ContainerUtil.createMaybeSingletonList(EDIT_MAPPINGS);
    }
    return new JsonSchemaInfoPopupStep(allSchemas, project, virtualFile, service, null);
  }
}
