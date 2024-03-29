// Copyright 2000-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.xmlb.annotations.Tag;
import com.intellij.util.xmlb.annotations.XCollection;
import fit.intellij.json.JsonBundle;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;

@State(name = "FitJsonSchemaMappingsProjectConfiguration", storages = @Storage("FitJsonSchemas.xml"))
public class JsonSchemaMappingsProjectConfiguration implements PersistentStateComponent<JsonSchemaMappingsProjectConfiguration.MyState> {
  @NotNull private final Project myProject;
  public volatile MyState myState = new MyState();

  @Nullable
  public fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration findMappingBySchemaInfo(JsonSchemaInfo value) {
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration : myState.myState.values()) {
      if (areSimilar(value, configuration)) return configuration;
    }
    return null;
  }

  public boolean areSimilar(JsonSchemaInfo value, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration) {
    return Objects.equals(normalizePath(value.getUrl(myProject)), normalizePath(configuration.getRelativePathToSchema()));
  }

  @Nullable
  @Contract("null -> null; !null -> !null")
  public String normalizePath(@Nullable String valueUrl) {
    if (valueUrl == null) return null;
    if (StringUtil.contains(valueUrl, "..")) {
      valueUrl = new File(valueUrl).getAbsolutePath();
    }
    return valueUrl.replace('\\', '/');
  }

  @Nullable
  public fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration findMappingForFile(VirtualFile file) {
    VirtualFile projectBaseDir = myProject.getBaseDir();
    for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration : myState.myState.values()) {
      for (fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item pattern : configuration.patterns) {
        if (pattern.mappingKind != JsonMappingKind.File) continue;
        VirtualFile relativeFile = VfsUtil.findRelativeFile(projectBaseDir, pattern.getPathParts());
        if (Objects.equals(relativeFile, file) || file.getUrl().equals(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration.Item.neutralizePath(pattern.getPath()))) {
          return configuration;
        }
      }
    }
    return null;
  }

  public static JsonSchemaMappingsProjectConfiguration getInstance(@NotNull final Project project) {
    return project.getService(JsonSchemaMappingsProjectConfiguration.class);
  }

  public JsonSchemaMappingsProjectConfiguration(@NotNull Project project) {
    myProject = project;
  }

  @Nullable
  @Override
  public MyState getState() {
    return myState;
  }

  public void schemaFileMoved(@NotNull final Project project,
                              @NotNull final String oldRelativePath,
                              @NotNull final String newRelativePath) {
      final Optional<fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> old = myState.myState.values().stream()
        .filter(schema -> FileUtil.pathsEqual(schema.getRelativePathToSchema(), oldRelativePath))
        .findFirst();
      old.ifPresent(configuration -> {
        configuration.setRelativePathToSchema(newRelativePath);
        JsonSchemaService.Impl.get(project).reset();
      });
  }

  public void removeConfiguration(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration) {
    for (Map.Entry<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> entry : myState.myState.entrySet()) {
      if (entry.getValue() == configuration) {
        myState.myState.remove(entry.getKey());
        return;
      }
    }
  }

  public void addConfiguration(fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration configuration) {
    String name = configuration.getName();
    while (myState.myState.containsKey(name)) {
      name += "1";
    }
    myState.myState.put(name, configuration);
  }

  public Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> getStateMap() {
    return Collections.unmodifiableMap(myState.myState);
  }

  @Override
  public void loadState(@NotNull MyState state) {
    myState = state;
    JsonSchemaService.Impl.get(myProject).reset();
  }

  public void setState(@NotNull Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> state) {
    myState = new MyState(state);
  }

  static class MyState {
    @Tag("state")
    @XCollection
    public Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> myState = new TreeMap<>();

    MyState() {
    }

    MyState(Map<String, fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration> state) {
      myState = state;
    }
  }

  public boolean isIgnoredFile(VirtualFile virtualFile) {
    fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration mappingForFile = findMappingForFile(virtualFile);
    return mappingForFile != null && mappingForFile.isIgnoredFile();
  }

  public void markAsIgnored(VirtualFile virtualFile) {
    fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration existingMapping = findMappingForFile(virtualFile);
    if (existingMapping != null) {
      removeConfiguration(existingMapping);
    }
    addConfiguration(createIgnoreSchema(virtualFile.getUrl()));
  }

  public void unmarkAsIgnored(VirtualFile virtualFile) {
    if (isIgnoredFile(virtualFile)) {
      fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration existingMapping = findMappingForFile(virtualFile);
      removeConfiguration(existingMapping);
    }
  }

  private static fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration createIgnoreSchema(String ignoredFileUrl) {
    fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration schemaConfiguration = new fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration(
      JsonBundle.message("schema.widget.no.schema.label"),
      JsonSchemaVersion.SCHEMA_4,
      "",
      true,
      Collections.singletonList(new UserDefinedJsonSchemaConfiguration.Item(ignoredFileUrl, false, false))
    );
    schemaConfiguration.setIgnoredFile(true);
    return schemaConfiguration;
  }
}