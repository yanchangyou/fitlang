/*
 * Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
 */
package fit.jetbrains.jsonSchema;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.xmlb.annotations.Tag;
import com.intellij.util.xmlb.annotations.XCollection;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
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
  public UserDefinedJsonSchemaConfiguration findMappingBySchemaInfo(fit.jetbrains.jsonSchema.extension.JsonSchemaInfo value) {
    for (UserDefinedJsonSchemaConfiguration configuration : myState.myState.values()) {
      if (areSimilar(value, configuration)) return configuration;
    }
    return null;
  }

  public boolean areSimilar(JsonSchemaInfo value, UserDefinedJsonSchemaConfiguration configuration) {
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
  public UserDefinedJsonSchemaConfiguration findMappingForFile(VirtualFile file) {
    VirtualFile projectBaseDir = myProject.getBaseDir();
    for (UserDefinedJsonSchemaConfiguration configuration : myState.myState.values()) {
      for (UserDefinedJsonSchemaConfiguration.Item pattern : configuration.patterns) {
        if (pattern.mappingKind != JsonMappingKind.File) continue;
        VirtualFile relativeFile = VfsUtil.findRelativeFile(projectBaseDir, pattern.getPathParts());
        if (Objects.equals(relativeFile, file) || file.getUrl().equals(UserDefinedJsonSchemaConfiguration.Item.neutralizePath(pattern.getPath()))) {
          return configuration;
        }
      }
    }
    return null;
  }

  public static JsonSchemaMappingsProjectConfiguration getInstance(@NotNull final Project project) {
    return ServiceManager.getService(project, JsonSchemaMappingsProjectConfiguration.class);
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
      final Optional<UserDefinedJsonSchemaConfiguration> old = myState.myState.values().stream()
        .filter(schema -> FileUtil.pathsEqual(schema.getRelativePathToSchema(), oldRelativePath))
        .findFirst();
      old.ifPresent(configuration -> {
        configuration.setRelativePathToSchema(newRelativePath);
        fit.jetbrains.jsonSchema.ide.JsonSchemaService.Impl.get(project).reset();
      });
  }

  public void removeConfiguration(UserDefinedJsonSchemaConfiguration configuration) {
    for (Map.Entry<String, UserDefinedJsonSchemaConfiguration> entry : myState.myState.entrySet()) {
      if (entry.getValue() == configuration) {
        myState.myState.remove(entry.getKey());
        return;
      }
    }
  }

  public void addConfiguration(UserDefinedJsonSchemaConfiguration configuration) {
    String name = configuration.getName();
    while (myState.myState.containsKey(name)) {
      name += "1";
    }
    myState.myState.put(name, configuration);
  }

  public Map<String, UserDefinedJsonSchemaConfiguration> getStateMap() {
    return Collections.unmodifiableMap(myState.myState);
  }

  @Override
  public void loadState(@NotNull MyState state) {
    myState = state;
    JsonSchemaService.Impl.get(myProject).reset();
  }

  public void setState(@NotNull Map<String, UserDefinedJsonSchemaConfiguration> state) {
    myState = new MyState(state);
  }

  static class MyState {
    @Tag("state")
    @XCollection
    public Map<String, UserDefinedJsonSchemaConfiguration> myState = new TreeMap<>();

    MyState() {
    }

    MyState(Map<String, UserDefinedJsonSchemaConfiguration> state) {
      myState = state;
    }
  }
}